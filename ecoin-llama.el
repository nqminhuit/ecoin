;;; ecoin-llama.el --- llama.cpp /infill backend for ecoin  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, completion
;; URL: https://github.com/nqminhuit/ecoin

;;; Commentary:

;; The `llama' backend of ecoin: fill-in-the-middle completions from a
;; llama.cpp server (`llama-server') over its /infill endpoint.  Nothing but
;; requests to `ecoin-llama-url' leaves the machine.
;;
;; Setup:
;;   llama-server --fim-qwen-1.5b-default      ; or any model with FIM tokens
;;   (setq ecoin-llama-url "http://127.0.0.1:8012"
;;         ecoin-llama-api-key "~/.llama-key")  ; only if the server wants one
;;
;; Design notes:
;; - One tiny HTTP/1.1 client on raw sockets (`ecoin-llama--http'): one
;;   connection per request, so cancelling a request is closing its socket.
;; - Server health is a state machine (`ecoin-llama--state'): a failure costs
;;   one message and a backoff, not an error per keystroke.
;; - At most one completion request is in flight across all buffers.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ecoin)

;; json.el is loaded only when Emacs has no native JSON.
(defvar json-false)
(defvar json-null)
(defvar json-object-type)
(defvar json-array-type)
(declare-function json-encode "json")
(declare-function json-read-from-string "json")

;;;; Customization

(defgroup ecoin-llama nil
  "The llama.cpp backend of ecoin."
  :group 'ecoin
  :prefix "ecoin-llama-")

(defcustom ecoin-llama-url "http://127.0.0.1:8012"
  "Root URL of the llama-server; only http is supported."
  :type 'string)

(defcustom ecoin-llama-api-key nil
  "API key of the server.
Nil, a string, the name of a file holding the key, or a function returning
a string.  Surrounding whitespace is ignored."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Key or file name")
                 function))

(defcustom ecoin-llama-n-prefix 256
  "Number of lines above the cursor line sent as context."
  :type 'integer)

(defcustom ecoin-llama-n-suffix 64
  "Number of lines below the cursor line sent as context."
  :type 'integer)

(defcustom ecoin-llama-max-prefix-chars 12000
  "Cap on the prefix context; whole lines are dropped from its top."
  :type 'integer)

(defcustom ecoin-llama-max-suffix-chars 4000
  "Cap on the suffix context; whole lines are dropped from its bottom."
  :type 'integer)

(defcustom ecoin-llama-n-predict 128
  "Maximum number of tokens the server generates."
  :type 'integer)

(defcustom ecoin-llama-t-max-predict-ms 250
  "Generation time after which the server stops at the next newline."
  :type 'integer)

(defcustom ecoin-llama-sampling
  '(:top_k 40 :top_p 0.9 :samplers ["top_k" "top_p" "infill"])
  "Sampling fields merged into every request body.
Sequences must be vectors, since nil serializes to an empty object."
  :type '(plist :key-type symbol :value-type sexp))

(defcustom ecoin-llama-line-suffix-regexp
  "\\`[ \t]*[])>}\"'`]*[ \t]*[:{;,]?[ \t]*\\'"
  "Automatic requests need the text after point to match this regexp.
It is the Emacs form of Copilot's /^\\s*[)>}\\]\"\\='`]*\\s*[:{;,]?\\s*$/.
Manual requests ignore it."
  :type 'regexp)

(defcustom ecoin-llama-slot nil
  "Integer to pin the server slot (id_slot), or nil to let the server pick."
  :type '(choice (const nil) integer))

(defcustom ecoin-llama-request-timeout 5
  "Seconds to wait for a normal request."
  :type 'number)

(defcustom ecoin-llama-wake-timeout 120
  "Seconds to wait for a request sent to a sleeping server."
  :type 'number)

(defcustom ecoin-llama-reprobe-after 60
  "Seconds since the last contact after which /props is probed again."
  :type 'number)

(defcustom ecoin-llama-retry-delay 0.1
  "Seconds between checks while a request is in flight."
  :type 'number)

(defcustom ecoin-llama-manual-alternatives 3
  "Completions asked for by `ecoin-complete', limited by the server's slots."
  :type 'integer)

(defcustom ecoin-llama-warn-non-loopback t
  "Non-nil: warn once per session when the URL host is not loopback."
  :type 'boolean)

;;;; State

(defconst ecoin-llama--failure-states
  '(unsupported unauthorized loading down error)
  "States that start a backoff.")

(defconst ecoin-llama--backoff-first 2 "Seconds of the first backoff.")
(defconst ecoin-llama--backoff-max 60 "Seconds of the longest backoff.")

(defvar ecoin-llama--state 'unknown
  "Server state; see the state machine in the spec of the backend.")
(defvar ecoin-llama--props nil "Plist parsed from the last /props answer.")
(defvar ecoin-llama--last-ok nil "`float-time' of the last successful contact.")
(defvar ecoin-llama--last-error nil "Text of the last failure.")
(defvar ecoin-llama--backoff nil "Current backoff in seconds, or nil.")
(defvar ecoin-llama--backoff-until nil "`float-time' at which the backoff ends.")
(defvar ecoin-llama--announced nil "(STATE . MESSAGE) last shown to the user.")
(defvar ecoin-llama--noted nil "Keys of one-time messages already shown.")
(defvar ecoin-llama--warned-loopback nil "Non-nil once the non-loopback warning ran.")
(defvar ecoin-llama--config-failed nil "Non-nil while the state is a config error.")
(defvar ecoin-llama--last-key nil "Last resolved API key, to notice changes.")
(defvar ecoin-llama--key-file-cache nil "(FILE MTIME . KEY) of the last key file read.")
(defvar ecoin-llama--inflight nil "The `ecoin-llama--job' being served.")
(defvar ecoin-llama--queued nil "The newest job waiting for the one in flight.")
(defvar ecoin-llama--retry-timer nil "Timer that starts the queued job.")
(defvar ecoin-llama--last-timings nil "Server timings of the last completion.")

(cl-defstruct (ecoin-llama--job (:constructor ecoin-llama--make-job))
  "A request from the core.  A nil CALLBACK means it was cancelled."
  buffer point tick callback ctx manual target conn done waking)

(defun ecoin-llama--reset (&optional keep-warned)
  "Forget the server state, backoff and requests; keep the warning if KEEP-WARNED."
  (when ecoin-llama--inflight (ecoin-llama--abort ecoin-llama--inflight))
  (when ecoin-llama--queued (setf (ecoin-llama--job-done ecoin-llama--queued) t))
  (when ecoin-llama--retry-timer (cancel-timer ecoin-llama--retry-timer))
  (setq ecoin-llama--state 'unknown
        ecoin-llama--props nil
        ecoin-llama--last-ok nil
        ecoin-llama--last-error nil
        ecoin-llama--backoff nil
        ecoin-llama--backoff-until nil
        ecoin-llama--announced nil
        ecoin-llama--noted nil
        ecoin-llama--config-failed nil
        ecoin-llama--last-key nil
        ecoin-llama--key-file-cache nil
        ecoin-llama--inflight nil
        ecoin-llama--queued nil
        ecoin-llama--retry-timer nil
        ecoin-llama--last-timings nil)
  (unless keep-warned (setq ecoin-llama--warned-loopback nil))
  (force-mode-line-update t))

(defun ecoin-llama--in-backoff-p ()
  "Non-nil while no automatic request may be sent."
  (and ecoin-llama--backoff-until
       (< (float-time) ecoin-llama--backoff-until)))

(defun ecoin-llama--set-state (state &optional message)
  "Enter STATE and show MESSAGE unless it was already shown for STATE.
A failure state also starts or lengthens the backoff.  The backoff and
the shown message are forgotten only by `ecoin-llama--note-success', so
a /props that answers while /infill keeps failing cannot reset them."
  (if (memq state ecoin-llama--failure-states)
      (setq ecoin-llama--last-error (or message (symbol-name state))
            ecoin-llama--backoff
            (if ecoin-llama--backoff
                (min ecoin-llama--backoff-max (* 2 ecoin-llama--backoff))
              ecoin-llama--backoff-first)
            ecoin-llama--backoff-until (+ (float-time) ecoin-llama--backoff))
    (when (memq state '(ready sleeping))
      (setq ecoin-llama--backoff-until nil
            ecoin-llama--last-ok (float-time))))
  (setq ecoin-llama--state state)
  (when (and message (not (equal (cons state message) ecoin-llama--announced)))
    (setq ecoin-llama--announced (cons state message))
    (message "%s" message))
  (force-mode-line-update t))

(defun ecoin-llama--note-success ()
  "The server completed a request: it is ready, and past failures are over."
  (setq ecoin-llama--backoff nil
        ecoin-llama--announced nil)
  (ecoin-llama--set-state 'ready))

(defun ecoin-llama--note-once (key message)
  "Show MESSAGE the first time KEY is seen this session."
  (unless (memq key ecoin-llama--noted)
    (push key ecoin-llama--noted)
    (message "%s" message)))

(defun ecoin-llama--config-error (text)
  "Report the configuration problem TEXT once; it clears when the config is fixed."
  (setq ecoin-llama--config-failed t
        ecoin-llama--state 'error
        ecoin-llama--last-error text)
  (unless (equal ecoin-llama--announced (cons 'error text))
    (setq ecoin-llama--announced (cons 'error text))
    (message "%s" text))
  (force-mode-line-update t))

(defun ecoin-llama--clear-failure ()
  "Forget a failure so the next request probes the server at once."
  (setq ecoin-llama--state 'unknown
        ecoin-llama--config-failed nil
        ecoin-llama--backoff nil
        ecoin-llama--backoff-until nil
        ecoin-llama--announced nil)
  (force-mode-line-update t))

;;;; Target: URL and API key

(cl-defstruct (ecoin-llama--target (:constructor ecoin-llama--make-target))
  "Where and how to talk to the server."
  host host-header port base key)

(defconst ecoin-llama--url-regexp
  "\\`\\(https?\\)://\\(\\[[^]]+\\]\\|[^/:?#]+\\)\\(?::\\([0-9]+\\)\\)?\\(/[^?#]*\\)?\\'"
  "Matches the URLs `ecoin-llama-url' may hold.")

(defun ecoin-llama--loopback-p (host)
  "Non-nil if HOST is a loopback address or name."
  (or (equal host "localhost")
      (equal host "::1")
      (string-match-p "\\`127\\.[0-9]+\\.[0-9]+\\.[0-9]+\\'" host)))

(defun ecoin-llama--key-from-file (file)
  "Return the contents of FILE, cached until its modification time changes."
  (let ((mtime (file-attribute-modification-time (file-attributes file))))
    (unless (and ecoin-llama--key-file-cache
                 (equal (car ecoin-llama--key-file-cache) file)
                 (equal (cadr ecoin-llama--key-file-cache) mtime))
      (setq ecoin-llama--key-file-cache
            (cons file
                  (cons mtime
                        (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))))))
    (cddr ecoin-llama--key-file-cache)))

(defun ecoin-llama--api-key ()
  "Return the trimmed API key, or nil for none.  Never include it in messages."
  (let* ((setting ecoin-llama-api-key)
         (raw (cond ((null setting) nil)
                    ((functionp setting) (funcall setting))
                    ((stringp setting)
                     ;; Not against `default-directory': in a TRAMP buffer that
                     ;; would stat a remote host on every request.
                     (let ((file (expand-file-name setting "~/")))
                       (if (and (file-readable-p file)
                                (not (file-directory-p file)))
                           (ecoin-llama--key-from-file file)
                         setting)))
                    (t (user-error "ecoin: ecoin-llama-api-key is invalid")))))
    (when raw
      (unless (stringp raw)
        (user-error "ecoin: ecoin-llama-api-key must give a string"))
      (let ((key (string-trim raw)))
        ;; Anything but printable ASCII would corrupt or split the header.
        (when (string-match-p "[^ -~]" key)
          (user-error
           "ecoin: the llama API key must be printable ASCII on one line"))
        (unless (string-empty-p key) key)))))

(defun ecoin-llama--target ()
  "Return the `ecoin-llama--target' for the current settings.
Signal a `user-error' when the URL or the key cannot be used."
  (let ((url ecoin-llama-url))
    (unless (and (stringp url) (string-match ecoin-llama--url-regexp url))
      (user-error "ecoin: ecoin-llama-url is not a valid http URL"))
    (when (equal (match-string 1 url) "https")
      (user-error "ecoin: https is not supported; use an http ecoin-llama-url"))
    (let* ((raw-host (match-string 2 url))
           (bracketed (string-prefix-p "[" raw-host))
           (host (if bracketed (substring raw-host 1 -1) raw-host))
           (port (if-let* ((p (match-string 3 url))) (string-to-number p) 80))
           (base (string-trim-right (or (match-string 4 url) "") "/+"))
           (key (ecoin-llama--api-key)))
      (when (and ecoin-llama-warn-non-loopback
                 (not ecoin-llama--warned-loopback)
                 (not (ecoin-llama--loopback-p host)))
        (setq ecoin-llama--warned-loopback t)
        (message "ecoin: %s is not a loopback address; code goes there over plain HTTP"
                 host))
      (unless (equal key ecoin-llama--last-key)
        (when (eq ecoin-llama--state 'unauthorized) (ecoin-llama--clear-failure))
        (setq ecoin-llama--last-key key))
      (when ecoin-llama--config-failed (ecoin-llama--clear-failure))
      (ecoin-llama--make-target :host host :port port :base base :key key
                                :host-header raw-host))))

;;;; Transport: raw-socket HTTP/1.1

(defconst ecoin-llama--http-buffer-name " *ecoin-llama-http*")
(defconst ecoin-llama--path-props "/props")
(defconst ecoin-llama--path-infill "/infill")
(defconst ecoin-llama--crlf "\r\n")

(cl-defstruct (ecoin-llama--conn (:constructor ecoin-llama--make-conn))
  "One HTTP request on its own socket."
  process buffer timer done callback)

(defvar ecoin-llama--native-json t
  "Tests bind this to nil to force the json.el code path.")

(defun ecoin-llama--native-json-p ()
  "Non-nil if Emacs's built-in JSON functions can be used."
  (and ecoin-llama--native-json
       (fboundp 'json-available-p)
       (json-available-p)))

(defun ecoin-llama--unibyte (string)
  "Return the encoded STRING with the unibyte flag set.
`encode-coding-string' leaves a pure-ASCII multibyte string as it is on
Emacs 28 and 29, and concatenating that would make the request multibyte."
  (if (multibyte-string-p string) (string-to-unibyte string) string))

(defun ecoin-llama--encode-body (object)
  "Serialize the plist OBJECT to a unibyte UTF-8 string."
  (ecoin-llama--unibyte
   (encode-coding-string
    (if (ecoin-llama--native-json-p)
        (json-serialize object)
      (require 'json)
      (let ((json-false :false) (json-null :null))
        (json-encode object)))
    'utf-8 t)))

(defun ecoin-llama--parse-json (string)
  "Parse the JSON STRING to plists and lists; nil for false, null or garbage."
  (condition-case nil
      (if (ecoin-llama--native-json-p)
          (json-parse-string string :object-type 'plist :array-type 'list
                             :null-object nil :false-object nil)
        (require 'json)
        (let ((json-object-type 'plist) (json-array-type 'list)
              (json-null nil) (json-false nil))
          (json-read-from-string string)))
    (error nil)))

(defun ecoin-llama--ascii (string)
  "Return STRING as unibyte ASCII; signal an error if it holds anything else."
  (when (string-match-p "[^\0-\177]" string)
    (error "Non-ASCII HTTP header"))
  (ecoin-llama--unibyte (encode-coding-string string 'us-ascii)))

(defun ecoin-llama--request-bytes (target method path payload)
  "Return the unibyte request for METHOD PATH on TARGET; PAYLOAD is unibyte or nil."
  (let* ((key (ecoin-llama--target-key target))
         (head (ecoin-llama--ascii
                (concat method " " (ecoin-llama--target-base target) path
                        " HTTP/1.1" ecoin-llama--crlf
                        "Host: " (ecoin-llama--target-host-header target) ":"
                        (number-to-string (ecoin-llama--target-port target))
                        ecoin-llama--crlf
                        (when payload
                          (concat "Content-Type: application/json"
                                  ecoin-llama--crlf))
                        "Accept: application/json" ecoin-llama--crlf
                        (when key
                          (concat "Authorization: Bearer " key ecoin-llama--crlf))
                        (when payload
                          (concat "Content-Length: "
                                  (number-to-string (length payload))
                                  ecoin-llama--crlf))
                        "Connection: close" ecoin-llama--crlf ecoin-llama--crlf)))
         (request (concat head payload)))
    (cl-assert (not (multibyte-string-p request)))
    request))

(defun ecoin-llama--http-dispose (conn)
  "Close CONN's socket and kill its buffer without running any callback."
  (when-let* ((proc (ecoin-llama--conn-process conn)))
    (set-process-filter proc #'ignore)
    (set-process-sentinel proc #'ignore)
    (delete-process proc))
  (when (buffer-live-p (ecoin-llama--conn-buffer conn))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer (ecoin-llama--conn-buffer conn))))
  (when (ecoin-llama--conn-timer conn)
    (cancel-timer (ecoin-llama--conn-timer conn)))
  (setf (ecoin-llama--conn-done conn) t))

(defun ecoin-llama--http-cancel (conn)
  "Abandon CONN: close the socket and never call its callback."
  (when (and conn (not (ecoin-llama--conn-done conn)))
    (ecoin-llama--http-dispose conn)))

(defun ecoin-llama--http-finish (conn result)
  "Close CONN and pass RESULT, a plist, to its callback."
  (unless (ecoin-llama--conn-done conn)
    (ecoin-llama--http-dispose conn)
    (condition-case err
        (funcall (ecoin-llama--conn-callback conn) result)
      (error (ecoin--log "http callback: %s" (car err))))))

(defun ecoin-llama--decode-chunked (bytes)
  "Return the body of the chunked BYTES, or nil if the last chunk is missing."
  (let ((pos 0) (parts nil))
    (catch 'done
      (while (and (string-match "\\([0-9a-fA-F]+\\)[^\r]*\r\n" bytes pos)
                  (= (match-beginning 0) pos))
        (let* ((size (string-to-number (match-string 1 bytes) 16))
               (start (match-end 0))
               (end (+ start size)))
          (cond ((= size 0) (throw 'done (apply #'concat (nreverse parts))))
                ((> (+ end 2) (length bytes)) (throw 'done nil))
                (t (push (substring bytes start end) parts)
                   (setq pos (+ end 2)))))))))

(defun ecoin-llama--http-parse (conn eof)
  "Finish CONN if a whole response has arrived; EOF means the peer closed."
  (let* ((raw (with-current-buffer (ecoin-llama--conn-buffer conn)
                (buffer-string)))
         (sep (string-search "\r\n\r\n" raw)))
    (if (not (and sep (string-match "\\`HTTP/1\\.[01] \\([0-9]+\\)" raw)))
        (when eof (ecoin-llama--http-finish conn '(:error broken)))
      (let* ((status (string-to-number (match-string 1 raw)))
             (head (substring raw 0 sep))
             (case-fold-search t)
             (length (and (string-match "^content-length: *\\([0-9]+\\)" head)
                          (string-to-number (match-string 1 head))))
             (chunked (string-match-p "^transfer-encoding:.*chunked" head))
             (rest (substring raw (+ sep 4)))
             (body (cond (chunked (ecoin-llama--decode-chunked rest))
                         (length (and (>= (string-bytes rest) length)
                                      (substring rest 0 length)))
                         (eof rest))))
        (cond (body
               (ecoin-llama--http-finish
                conn
                (list :status status
                      :body (and (> (length body) 0)
                                 (ecoin-llama--parse-json
                                  (decode-coding-string body 'utf-8))))))
              (eof (ecoin-llama--http-finish conn '(:error broken))))))))

(defun ecoin-llama--http (target method path object timeout callback)
  "Send METHOD PATH to TARGET with OBJECT (a plist or nil) as the JSON body.
CALLBACK gets (:status N :body PLIST) or (:error refused|broken|timeout)
once, unless the returned connection is cancelled."
  (let* ((payload (and object (ecoin-llama--encode-body object)))
         (request (ecoin-llama--request-bytes target method path payload))
         (buffer (generate-new-buffer ecoin-llama--http-buffer-name t))
         (conn (ecoin-llama--make-conn :buffer buffer :callback callback)))
    (with-current-buffer buffer (set-buffer-multibyte nil))
    (setf (ecoin-llama--conn-timer conn)
          (run-at-time timeout nil #'ecoin-llama--http-finish conn
                       '(:error timeout)))
    (condition-case nil
        (setf (ecoin-llama--conn-process conn)
              (make-network-process
               :name "ecoin-llama" :buffer buffer
               :host (ecoin-llama--target-host target)
               :service (ecoin-llama--target-port target)
               :nowait t :coding 'binary :noquery t
               :filter
               (lambda (_proc string)
                 (when (and (not (ecoin-llama--conn-done conn))
                            (buffer-live-p buffer))
                   (with-current-buffer buffer (insert string))
                   (ecoin-llama--http-parse conn nil)))
               :sentinel
               (lambda (proc event)
                 (unless (ecoin-llama--conn-done conn)
                   (cond ((string-prefix-p "open" event)
                          (condition-case nil
                              (process-send-string proc request)
                            (error (ecoin-llama--http-finish
                                    conn '(:error broken)))))
                         ((string-prefix-p "failed" event)
                          (ecoin-llama--http-finish conn '(:error refused)))
                         (t (ecoin-llama--http-parse conn t)))))))
      (error (ecoin-llama--http-finish conn '(:error refused))))
    conn))

;;;; Jobs: one completion request, from trigger to callback

(defun ecoin-llama--abort (job)
  "Close JOB's socket for good."
  (ecoin-llama--http-cancel (ecoin-llama--job-conn job))
  (setf (ecoin-llama--job-done job) t)
  (when (eq job ecoin-llama--inflight) (setq ecoin-llama--inflight nil)))

(defun ecoin-llama--finish (job)
  "Mark JOB served; the retry timer starts the next one."
  (setf (ecoin-llama--job-done job) t
        (ecoin-llama--job-conn job) nil)
  (when (eq job ecoin-llama--inflight) (setq ecoin-llama--inflight nil))
  (when ecoin-llama--queued (ecoin-llama--arm-retry)))

(defun ecoin-llama--arm-retry ()
  "Make sure the queued job is looked at again soon, without an idle timer."
  (unless ecoin-llama--retry-timer
    (setq ecoin-llama--retry-timer
          (run-with-timer ecoin-llama-retry-delay nil #'ecoin-llama--retry))))

(defun ecoin-llama--retry ()
  "Start the queued job if nothing is in flight."
  (setq ecoin-llama--retry-timer nil)
  (when-let* ((job ecoin-llama--queued))
    (cond ((ecoin-llama--job-done job) (setq ecoin-llama--queued nil))
          (ecoin-llama--inflight (ecoin-llama--arm-retry))
          (t (setq ecoin-llama--queued nil)
             (ecoin-llama--start job)))))

(defun ecoin-llama--far-p (old new)
  "Non-nil if job NEW is in another buffer than OLD or too far from it."
  (or (not (eq (ecoin-llama--job-buffer old) (ecoin-llama--job-buffer new)))
      (let ((buf (ecoin-llama--job-buffer old)))
        (and (buffer-live-p buf)
             (with-current-buffer buf
               (save-restriction
                 (widen)
                 (let ((a (min (point-max) (ecoin-llama--job-point old)))
                       (b (min (point-max) (ecoin-llama--job-point new))))
                   (> (count-lines (min a b) (max a b))
                      ecoin-llama-n-suffix))))))))

(defun ecoin-llama--submit (job)
  "Start JOB now, or queue it behind the request in flight."
  (when-let* ((cur ecoin-llama--inflight))
    (when (ecoin-llama--far-p cur job) (ecoin-llama--abort cur)))
  (when ecoin-llama--queued
    (setf (ecoin-llama--job-done ecoin-llama--queued) t
          ecoin-llama--queued nil))
  (if ecoin-llama--inflight
      (progn (setq ecoin-llama--queued job)
             (ecoin-llama--arm-retry))
    (ecoin-llama--start job)))

(defun ecoin-llama--needs-probe-p ()
  "Non-nil if /props must answer before a completion is sent."
  (or (not (memq ecoin-llama--state '(ready sleeping)))
      (null ecoin-llama--last-ok)
      (> (- (float-time) ecoin-llama--last-ok) ecoin-llama-reprobe-after)))

(defun ecoin-llama--start (job)
  "Serve JOB: probe the server if needed, then send the completion."
  (cond
   ((or (ecoin-llama--job-done job) (null (ecoin-llama--job-callback job)))
    (setf (ecoin-llama--job-done job) t))
   ;; A job that waited out a failure must not probe before the backoff ends.
   ((and (not (ecoin-llama--job-manual job)) (ecoin-llama--in-backoff-p))
    (setf (ecoin-llama--job-done job) t))
   (t
    (setq ecoin-llama--inflight job)
    (if (ecoin-llama--needs-probe-p)
        (ecoin-llama--probe job #'ecoin-llama--send)
      (ecoin-llama--send job)))))

;;;; Server answers

(defun ecoin-llama--error-message (body)
  "The server's error text in BODY, shortened."
  (let ((msg (plist-get (plist-get body :error) :message)))
    (if (stringp msg) (truncate-string-to-width msg 200 nil nil "...") "")))

(defun ecoin-llama--http-failure (status body)
  "Update the state for an unsuccessful STATUS with parsed BODY."
  (let ((msg (ecoin-llama--error-message body)))
    (pcase status
      (501 (ecoin-llama--set-state
            'unsupported
            (format "ecoin: model at %s has no FIM tokens; load a FIM model or set `ecoin-backend'"
                    ecoin-llama-url)))
      (401 (ecoin-llama--set-state
            'unauthorized
            "ecoin: llama server rejected the API key; check `ecoin-llama-api-key'"))
      (503 (ecoin-llama--set-state
            'loading "ecoin: llama server is loading a model"))
      ((and 500 (guard (string-match-p "Context size has been exceeded" msg)))
       (ecoin-llama--note-success)
       (ecoin-llama--note-once
        'context "ecoin: llama server ran out of context; no completion"))
      (_ (ecoin-llama--set-state
          'error (format "ecoin: llama server answered %d%s" status
                         (if (string-empty-p msg) "" (concat ": " msg))))))))

(defun ecoin-llama--down ()
  "Record that the server cannot be reached."
  (ecoin-llama--set-state
   'down (format "ecoin: llama server unreachable at %s" ecoin-llama-url)))

(defun ecoin-llama--probe (job next)
  "GET /props for JOB, then call NEXT with JOB if the server answers."
  (setf (ecoin-llama--job-conn job)
        (ecoin-llama--http
         (ecoin-llama--job-target job) "GET" ecoin-llama--path-props nil
         ecoin-llama-request-timeout
         (lambda (result)
           (setf (ecoin-llama--job-conn job) nil)
           (let ((status (plist-get result :status)))
             (cond
              ((null status) (ecoin-llama--down) (ecoin-llama--finish job))
              ((/= status 200)
               (ecoin-llama--http-failure status (plist-get result :body))
               (ecoin-llama--finish job))
              (t
               (let ((props (plist-get result :body)))
                 (setq ecoin-llama--props props)
                 (ecoin-llama--set-state
                  (if (plist-get props :is_sleeping) 'sleeping 'ready)))
               (if (ecoin-llama--job-callback job)
                   (funcall next job)
                 (ecoin-llama--finish job)))))))))

(defun ecoin-llama--alternatives (job)
  "Number of completions to ask for in JOB."
  (if (ecoin-llama--job-manual job)
      (max 1 (min ecoin-llama-manual-alternatives
                  (or (plist-get ecoin-llama--props :total_slots) 1)))
    1))

(defconst ecoin-llama--response-fields
  ["content" "stop_type" "truncated" "tokens_cached" "timings/prompt_n"
   "timings/prompt_ms" "timings/predicted_n" "timings/predicted_ms"
   "timings/cache_n"]
  "Fields of the /infill answer that ecoin reads.")

(defconst ecoin-llama--forbidden-fields '(:t_max_prompt_ms :n_cache_reuse :model)
  "Fields that must never be sent, even from `ecoin-llama-sampling'.")

(defun ecoin-llama--request-body (job)
  "Build the /infill request plist for JOB."
  (let* ((ctx (ecoin-llama--job-ctx job))
         (n (ecoin-llama--alternatives job))
         (body (append
                (list :input_prefix (plist-get ctx :prefix)
                      :input_suffix (plist-get ctx :suffix)
                      :prompt (plist-get ctx :middle)
                      :input_extra []
                      :n_predict ecoin-llama-n-predict
                      :n_indent (plist-get ctx :n-indent)
                      :t_max_predict_ms ecoin-llama-t-max-predict-ms
                      :stream :false
                      :cache_prompt t
                      :response_fields ecoin-llama--response-fields)
                (when ecoin-llama-slot (list :id_slot ecoin-llama-slot))
                (when (> n 1) (list :n_cmpl n))))
         (extra nil))
    (cl-loop for (key value) on ecoin-llama-sampling by #'cddr
             unless (or (memq key ecoin-llama--forbidden-fields)
                        (and (eq key :stop) (zerop (length value)))
                        (plist-member body key))
             do (setq extra (append extra (list key value))))
    (append body extra)))

(defun ecoin-llama--send (job)
  "Send JOB's completion request."
  (let ((wake (eq ecoin-llama--state 'sleeping)))
    (when wake
      (setf (ecoin-llama--job-waking job) t)
      (ecoin-llama--set-state 'sleeping "ecoin: waking llama server..."))
    (setf (ecoin-llama--job-conn job)
          (ecoin-llama--http
           (ecoin-llama--job-target job) "POST" ecoin-llama--path-infill
           (ecoin-llama--request-body job)
           (if wake ecoin-llama-wake-timeout ecoin-llama-request-timeout)
           (lambda (result) (ecoin-llama--on-infill job result))))))

(defun ecoin-llama--on-infill (job result)
  "Handle RESULT of JOB's /infill request."
  (setf (ecoin-llama--job-conn job) nil)
  (let ((status (plist-get result :status))
        (error (plist-get result :error)))
    (cond
     ((eq error 'timeout)
      ;; A slow answer is not proof of a dead server; ask /props.
      (ecoin-llama--probe job #'ecoin-llama--after-timeout))
     (error (ecoin-llama--down) (ecoin-llama--finish job))
     ((= status 200)
      (ecoin-llama--note-success)
      (ecoin-llama--deliver job (plist-get result :body))
      (ecoin-llama--finish job))
     (t (ecoin-llama--http-failure status (plist-get result :body))
        (ecoin-llama--finish job)))))

(defun ecoin-llama--after-timeout (job)
  "The server answered /props after JOB's request timed out."
  (if (and (eq ecoin-llama--state 'sleeping)
           (not (ecoin-llama--job-waking job)))
      (ecoin-llama--send job)
    (when (eq ecoin-llama--state 'sleeping)
      (ecoin-llama--set-state
       'error "ecoin: llama server did not wake up in time"))
    (ecoin-llama--finish job)))

(defun ecoin-llama--contents (body)
  "The completion strings in the parsed /infill answer BODY."
  (let ((objects (cond ((keywordp (car-safe body)) (list body))
                       ((consp body) body))))
    (seq-filter #'stringp (mapcar (lambda (o) (plist-get o :content)) objects))))

(defun ecoin-llama--deliver (job body)
  "Post-process BODY and hand the items to JOB's callback if still current."
  (let ((buffer (ecoin-llama--job-buffer job))
        (callback (ecoin-llama--job-callback job)))
    (setq ecoin-llama--last-timings
          (list :prompt_n (plist-get body :timings/prompt_n)
                :prompt_ms (plist-get body :timings/prompt_ms)
                :predicted_n (plist-get body :timings/predicted_n)
                :predicted_ms (plist-get body :timings/predicted_ms)
                :cache_n (plist-get body :timings/cache_n)))
    (ecoin--log "infill: %s" ecoin-llama--last-timings)
    (when (and callback (buffer-live-p buffer))
      (with-current-buffer buffer
        (when (and (= (point) (ecoin-llama--job-point job))
                   (= (buffer-chars-modified-tick) (ecoin-llama--job-tick job)))
          (condition-case err
              (funcall callback (ecoin-llama--items (ecoin-llama--contents body)))
            (error (ecoin--log "deliver: %s" (car err)))))))))

;;;; Context

(defun ecoin-llama--cut-top (text max)
  "Drop whole lines from the top of TEXT until at most MAX chars remain."
  (if (<= (length text) max)
      text
    (let ((start (- (length text) max)))
      (if (eq (aref text (1- start)) ?\n)
          (substring text start)
        (if-let* ((nl (string-search "\n" text start)))
            (substring text (1+ nl))
          "")))))

(defun ecoin-llama--cut-bottom (text max)
  "Drop whole lines from the bottom of TEXT until at most MAX chars remain."
  (if (<= (length text) max)
      text
    (let ((nl (cl-position ?\n (substring text 0 max) :from-end t)))
      (if nl (substring text 0 (1+ nl)) ""))))

(defun ecoin-llama--context ()
  "Return the /infill context around point as a plist.
Keys: :prefix :middle :suffix :n-indent :text-before :text-after."
  (save-restriction
    (widen)
    (save-excursion
      (let* ((inhibit-field-text-motion t)
             (pt (point))
             (bol (progn (forward-line 0) (point)))
             (eol (line-end-position))
             (line (buffer-substring-no-properties bol eol))
             (text-before (buffer-substring-no-properties bol pt))
             (text-after (buffer-substring-no-properties pt eol))
             (blank (string-match-p "\\`[ \t]*\\'" line))
             (above (buffer-substring-no-properties
                     (progn (forward-line (- ecoin-llama-n-prefix)) (point))
                     bol))
             (below (buffer-substring-no-properties
                     (progn (goto-char eol) (forward-line 1) (point))
                     (progn (forward-line ecoin-llama-n-suffix) (point)))))
        (when (and (> (length below) 0)
                   (not (string-suffix-p "\n" below)))
          (setq below (concat below "\n")))
        (list :prefix (ecoin-llama--cut-top above ecoin-llama-max-prefix-chars)
              :middle (if blank "" text-before)
              :suffix (concat text-after "\n"
                              (ecoin-llama--cut-bottom
                               below ecoin-llama-max-suffix-chars))
              :n-indent (if blank 0
                          (string-match "\\`[ \t]*" line)
                          (match-end 0))
              :text-before text-before
              :text-after text-after)))))

;;;; Post-processing

(defconst ecoin-llama--leak-regexp
  "<|\\(?:endoftext\\|file_sep\\|fim_[a-z_]*\\|im_end\\|cursor\\)|>"
  "FIM and control tokens that must never reach the buffer.")

(defconst ecoin-llama--closer-regexp
  (concat "\\`[ \t]*\\(?:[])}>]+[;,]?\\|end\\|fi\\|done\\|esac\\|endif"
          "\\|endfor\\|endwhile\\|endfunction\\|end;\\|end,\\)[ \t]*\\'")
  "A line that only closes a block.")

(defconst ecoin-llama--repeat-regexps
  '("\\(.\\{3,\\}\\)\\1\\{5,\\}$" "\\(.\\{10,\\}\\)\\1\\{3,\\}$")
  "Degenerate repetition at the end of a line (Tabby).")

(defun ecoin-llama--lines-after (n)
  "The N lines below the current one, without newlines."
  (save-restriction
    (widen)
    (save-excursion
      (let ((inhibit-field-text-motion t) (lines nil))
        (end-of-line)
        (while (and (> n 0) (not (eobp)))
          (forward-line 1)
          (push (buffer-substring-no-properties (point) (line-end-position))
                lines)
          (setq n (1- n)))
        (nreverse lines)))))

(defun ecoin-llama--discard-p (lines text-before text-after next)
  "Non-nil if the suggestion LINES only repeats the buffer.
TEXT-BEFORE and TEXT-AFTER are the current line around point, NEXT the
lines below it (the llama.vim rules)."
  (let ((first (car lines)) (n (length lines)))
    (or
     (and (= n 1)
          (or (equal first text-after)
              (equal (string-trim first) (string-trim text-after))))
     (and (equal first "") (cdr lines)
          (equal (cdr lines) (seq-take next (1- n))))
     (let* ((tail (cl-member-if-not #'string-blank-p next))
            (after (cdr tail)))
       (and tail
            (equal (concat text-before first) (car tail))
            (or (= n 1)
                (and (= n 2) after
                     (string-prefix-p (cadr lines) (car after)))
                (and (> n 2) (equal (cdr lines) (seq-take after (1- n))))))))))

(defun ecoin-llama--cut-repeats (lines)
  "Cut LINES at the second of two consecutive identical non-blank lines."
  (let ((out (list (car lines))) (prev (car lines)))
    (catch 'done
      (dolist (l (cdr lines))
        (when (and (equal l prev) (not (string-blank-p l)))
          (throw 'done nil))
        (push l out)
        (setq prev l)))
    (nreverse out)))

(defun ecoin-llama--snip-closers (lines next)
  "Drop trailing closer-only LINES that the buffer lines NEXT already hold.
The first of LINES continues the current line and is never dropped."
  (let ((following (seq-remove #'string-blank-p next))
        (tail nil)
        (rest (reverse (cdr lines))))
    ;; The trailing non-blank closer-only lines, in buffer order.
    (while (and rest (or (string-blank-p (car rest))
                         (string-match-p ecoin-llama--closer-regexp (car rest))))
      (unless (string-blank-p (car rest)) (push (car rest) tail))
      (pop rest))
    (let ((k (length tail)))
      (while (and (> k 0)
                  (not (equal (last tail k) (seq-take following k))))
        (setq k (1- k)))
      (if (= k 0)
          lines
        ;; Remove the last K non-blank lines and the blanks before them.
        (let ((src (reverse lines)))
          (while (and src (or (> k 0) (string-blank-p (car src))))
            (unless (string-blank-p (car src)) (setq k (1- k)))
            (pop src))
          (nreverse src))))))

(defun ecoin-llama--postprocess (content &optional keep-indent)
  "Turn the raw server CONTENT into ghost text for point, or nil for none.
With KEEP-INDENT, put back the indentation removed from a whitespace-only
line, since the core's dedent step on delivery removes it."
  (let* ((text (replace-regexp-in-string ecoin-llama--leak-regexp "" content t t))
         (rev (reverse (split-string text "\n")))
         (text-before (buffer-substring-no-properties
                       (line-beginning-position) (point)))
         (text-after (buffer-substring-no-properties
                       (point) (line-end-position)))
         (blank-line (string-blank-p (concat text-before text-after)))
         (removed ""))
    (while (and rev (string-blank-p (car rev))) (pop rev))
    (when rev
      (setcar rev (string-trim-right (car rev)))
      (let* ((lines (nreverse rev))
             (next (ecoin-llama--lines-after (+ 200 (length lines)))))
        (when (and blank-line (string-match "\\`[ \t]+" (car lines)))
          (let ((k (min (match-end 0) (length text-before))))
            (setq removed (substring (car lines) 0 k))
            (setcar lines (substring (car lines) k))))
        (unless (ecoin-llama--discard-p lines text-before text-after next)
          (setq lines (ecoin-llama--cut-repeats lines))
          (unless (cl-some (lambda (re) (string-match-p re (car (last lines))))
                           ecoin-llama--repeat-regexps)
            (when (string-match-p "[^ \t]" text-after)
              (let ((first (car lines))
                    (tail (string-trim text-after)))
                (when (and (> (length tail) 0) (string-suffix-p tail first))
                  (setq first (substring first 0 (- (length first)
                                                    (length tail)))))
                (setq lines (list first))))
            (when (cdr lines)
              (setq lines (ecoin-llama--snip-closers lines next)))
            (let ((out (string-join lines "\n")))
              (when (string-match-p "[^ \t\n]" out)
                (concat (and keep-indent removed) out)))))))))

(defun ecoin-llama--items (contents)
  "Convert raw CONTENTS to `ecoin-item's for point; drop empties and duplicates."
  (let ((seen nil) (items nil))
    (dolist (c contents)
      (when-let* ((text (ecoin-llama--postprocess c t)))
        (let ((key (string-trim text)))
          (unless (member key seen)
            (push key seen)
            (push (ecoin-make-item :text text :backend 'llama) items)))))
    (nreverse items)))

;;;; Backend methods

(defun ecoin-llama--auto-ok-p ()
  "Non-nil if an automatic request may be made at point."
  (and (not (ecoin-llama--in-backoff-p))
       (string-match-p ecoin-llama-line-suffix-regexp
                       (buffer-substring-no-properties
                        (point) (line-end-position)))))

(defun ecoin-llama--target-or-nil (manual)
  "Return the target; on a config error signal if MANUAL, else report once."
  (condition-case err
      (ecoin-llama--target)
    (user-error
     (if manual
         (signal (car err) (cdr err))
       (ecoin-llama--config-error (cadr err))
       nil))))

(cl-defmethod ecoin-backend-request ((_backend (eql 'llama)) request callback)
  "Ask the server for completions for REQUEST; CALLBACK gets the items."
  (let* ((manual (eq (ecoin-request-trigger request) 'manual))
         (target (ecoin-llama--target-or-nil manual)))
    (when (and target (or manual (ecoin-llama--auto-ok-p)))
      (let ((job (ecoin-llama--make-job
                  :buffer (ecoin-request-buffer request)
                  :point (ecoin-request-point request)
                  :tick (ecoin-request-tick request)
                  :callback callback
                  :ctx (ecoin-llama--context)
                  :manual manual
                  :target target)))
        (ecoin-llama--submit job)
        job))))

(cl-defmethod ecoin-backend-cancel ((_backend (eql 'llama)) job)
  "Detach JOB from its callback.
The socket stays open: the core cancels on every keystroke, and the single
flight finishes the request anyway."
  (when (ecoin-llama--job-p job)
    (setf (ecoin-llama--job-callback job) nil)
    (when (eq job ecoin-llama--queued)
      (setf (ecoin-llama--job-done job) t
            ecoin-llama--queued nil))))

(cl-defmethod ecoin-backend-disable-buffer ((_backend (eql 'llama)))
  "Close the request in flight for the current buffer, if any."
  (let ((queued ecoin-llama--queued))
    (when (and queued (eq (ecoin-llama--job-buffer queued) (current-buffer)))
      (setf (ecoin-llama--job-done queued) t)
      (setq ecoin-llama--queued nil)))
  (let ((job ecoin-llama--inflight))
    (when (and job (eq (ecoin-llama--job-buffer job) (current-buffer)))
      (setf (ecoin-llama--job-callback job) nil)
      (ecoin-llama--abort job))))

(cl-defmethod ecoin-backend-capabilities ((_backend (eql 'llama)))
  "Llama requests are cheap enough to follow the cursor."
  '(:trigger-on-move t))

(cl-defmethod ecoin-backend-mode-line ((_backend (eql 'llama)))
  "Mode-line marker for the server state."
  (pcase ecoin-llama--state
    ('sleeping "[z]")
    ((or 'unsupported 'unauthorized 'down 'error) "[!]")))

(cl-defmethod ecoin-backend-available-p ((_backend (eql 'llama)))
  "Nil while a failure is fresh; a sleeping server counts, a request wakes it.
Sends nothing.  A config error is rechecked here, since fixing it needs no
network."
  (cond
   (ecoin-llama--config-failed
    (condition-case nil (progn (ecoin-llama--target) t) (user-error nil)))
   ((memq ecoin-llama--state ecoin-llama--failure-states)
    (not (ecoin-llama--in-backoff-p)))
   (t t)))

(cl-defmethod ecoin-backend-unavailable-reason ((_backend (eql 'llama)))
  "Short phrase for the current failure state."
  (pcase ecoin-llama--state
    ('down "unreachable")
    ('loading "loading a model")
    ('unsupported "model has no FIM tokens")
    ('unauthorized "API key rejected")
    ('error (if ecoin-llama--config-failed "configuration error" "error"))))

(cl-defmethod ecoin-backend-restart ((_backend (eql 'llama)))
  "Forget the server state and backoff; the next request probes again."
  (ecoin-llama--reset t)
  (message "ecoin: llama state reset"))

(cl-defmethod ecoin-backend-status ((_backend (eql 'llama)))
  "Describe the server and the state of the connection."
  (let ((model (or (plist-get ecoin-llama--props :model_alias)
                   (plist-get ecoin-llama--props :model_path))))
    (format "backend llama; state %s; url %s; model %s; slots %s; last error %s; backoff %s"
            ecoin-llama--state ecoin-llama-url (or model "unknown")
            (or (plist-get ecoin-llama--props :total_slots) "unknown")
            (or ecoin-llama--last-error "none")
            (if (ecoin-llama--in-backoff-p)
                (format "%d s left"
                        (ceiling (- ecoin-llama--backoff-until (float-time))))
              "none"))))

(provide 'ecoin-llama)
;;; ecoin-llama.el ends here
