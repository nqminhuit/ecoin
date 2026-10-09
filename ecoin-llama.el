;;; ecoin-llama.el --- llama.cpp /infill backend for ecoin  -*- lexical-binding: t; -*-

;; Version: 0.2.0
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
;; - An LRU cache of raw answers (also under keys that survive the prefix
;;   window shifting) serves repeated and typed-through contexts without a
;;   request.  After a ghost is shown, a speculative request for the context
;;   after accepting it fills the cache; it shares the single flight, so a
;;   user request waits for it.  Background requests need a recent trigger.
;; - A per-project ring of code chunks (other files, saves, copies, far parts
;;   of the buffer) goes out as `input_extra'.  The server puts it first in the
;;   prompt, so a warm-up request keeps it in the KV cache.  Cached answers are
;;   keyed by a hash of the extra text, so a changed ring never serves an
;;   answer computed with other context.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-util)
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

(defcustom ecoin-llama-model nil
  "Model id the server routes requests by, or nil to send none.
Nil suits a single-model server; a llama.cpp router needs a string."
  :type '(choice (const nil) string))

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

(defcustom ecoin-llama-cache-size 250
  "Number of contexts whose raw completions are kept, least recently used out."
  :type 'integer)

(defcustom ecoin-llama-prefetch t
  "Non-nil: after a ghost is shown, request the next one as if it were accepted.
The answer only fills the cache."
  :type 'boolean)

(defcustom ecoin-llama-prefetch-t-max-predict-ms 500
  "Generation time budget of a speculative prefetch request.
Lower than for a user request because a prefetch holds the single flight."
  :type 'integer)

(defcustom ecoin-llama-activity-window 30
  "Seconds after the last trigger during which background requests are sent."
  :type 'number)

(defcustom ecoin-llama-ring-chunks 16
  "Extra-context chunks kept per project; 0 turns the ring off."
  :type 'integer)

(defcustom ecoin-llama-ring-chunk-lines 32
  "Lines per extra-context chunk."
  :type 'integer)

(defcustom ecoin-llama-extra-max-chars 24000
  "Cap on the text of all `input_extra' chunks of one request, oldest dropped."
  :type 'integer)

(defcustom ecoin-llama-ring-interval 1.0
  "Seconds between ring updates (one queued chunk, one warm-up request)."
  :type 'number)

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
(defvar ecoin-llama--last-model nil "Last `ecoin-llama-model', to notice changes.")
(defvar ecoin-llama--key-file-cache nil "(FILE MTIME . KEY) of the last key file read.")
(defvar ecoin-llama--inflight nil "The `ecoin-llama--job' being served.")
(defvar ecoin-llama--queued nil "The newest job waiting for the one in flight.")
(defvar ecoin-llama--retry-timer nil "Timer that starts the queued job.")
(defvar ecoin-llama--last-timings nil "Server timings of the last completion.")
(defvar ecoin-llama--last-activity nil
  "`float-time' of the last user trigger; background requests need it recent.")
;; Statistics for `ecoin-stats': counters, and the server timings of the last
;; `ecoin--stats-size' completions.
(defvar ecoin-llama--stat-counts nil "Alist (KEY . COUNT) of cache and request counters.")
(defvar ecoin-llama--stat-timings (ecoin--make-ring ecoin--stats-size)
  "Ring of plists of server timings, one per completion a user asked for.")
(defvar ecoin-llama--prefetch-timer nil "Timer that sends the pending prefetch.")
(defvar ecoin-llama--rings (make-hash-table :test #'equal)
  "Project root to its `ecoin-llama--ring'.")
(defvar ecoin-llama--halvings 0
  "Times a \"context exceeded\" answer halved the extra-context cap.")
(defvar ecoin-llama--ring-timer nil "Repeating timer of the ring updates.")
(defvar ecoin-llama--switch-timer nil "Timer that handles a window change.")
(defvar ecoin-llama--prev-buffer nil "Buffer that was selected at the last switch.")
(defvar ecoin-llama--kill-source nil "(BUFFER . TEXT) of the last copy or kill.")
(defvar ecoin-llama--seen-kill nil "The `kill-ring' head the ring timer saw last.")
(defvar ecoin-llama--last-ring nil "(RING . BUFFER) of the latest completion request.")
(defvar ecoin-llama--warmed nil "Id of the extra context the last warm-up carried.")
(defvar ecoin-llama--last-warmup nil "Plist (:chunks :chars :ms) of the last warm-up.")

(cl-defstruct (ecoin-llama--job (:constructor ecoin-llama--make-job))
  "A request from the core.  A nil CALLBACK means it was cancelled."
  buffer point tick callback ctx manual target conn done waking
  prefetch                              ; fills the cache only
  warmup                                ; fills the server's KV cache only
  ring                                  ; the `ecoin-llama--ring' it sends
  extra                                 ; the `input_extra' vector it sent
  sent                                  ; `float-time' of the send
  waited)                               ; queued behind another request

(defun ecoin-llama--reset (&optional keep-warned)
  "Forget the server state, backoff and requests; keep the warning if KEEP-WARNED."
  (when ecoin-llama--inflight (ecoin-llama--abort ecoin-llama--inflight))
  (when ecoin-llama--queued (setf (ecoin-llama--job-done ecoin-llama--queued) t))
  (when ecoin-llama--retry-timer (cancel-timer ecoin-llama--retry-timer))
  (when ecoin-llama--prefetch-timer (cancel-timer ecoin-llama--prefetch-timer))
  (when ecoin-llama--ring-timer (cancel-timer ecoin-llama--ring-timer))
  (when ecoin-llama--switch-timer (cancel-timer ecoin-llama--switch-timer))
  (clrhash ecoin-llama--rings)
  (ecoin-llama--cache-clear)
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
        ecoin-llama--prefetch-timer nil
        ecoin-llama--ring-timer nil
        ecoin-llama--switch-timer nil
        ecoin-llama--halvings 0
        ecoin-llama--prev-buffer nil
        ecoin-llama--kill-source nil
        ecoin-llama--seen-kill nil
        ecoin-llama--last-ring nil
        ecoin-llama--warmed nil
        ecoin-llama--last-warmup nil
        ecoin-llama--last-activity nil
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

(defun ecoin-llama--sync-model ()
  "Reset the server state if `ecoin-llama-model' changed; signal if it is invalid.
Cached completions, the warm-up and /props belong to the previous model."
  (let ((model ecoin-llama-model))
    (when (and model (not (and (stringp model) (not (string-blank-p model)))))
      (user-error "ecoin: ecoin-llama-model must be nil or a non-blank string"))
    (unless (equal model ecoin-llama--last-model)
      (ecoin-llama--reset t)
      (setq ecoin-llama--last-model model))))

(defun ecoin-llama--target ()
  "Return the `ecoin-llama--target' for the current settings.
Signal a `user-error' when the URL, the key or the model cannot be used."
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
           ;; Before the key: a reset forgets `ecoin-llama--last-key'.
           (_ (ecoin-llama--sync-model))
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
(defconst ecoin-llama--props-query "?model=%s&autoload=false"
  "Query that asks a router for one model's /props without loading it.")
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

(defun ecoin-llama--background-job-p (job)
  "Non-nil if JOB is a prefetch or warm-up: nobody waits for its answer."
  (or (ecoin-llama--job-prefetch job) (ecoin-llama--job-warmup job)))

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
    ;; A prefetch is never cancelled by a trigger; the trigger waits for it.
    (when (and (not (ecoin-llama--background-job-p cur))
               (ecoin-llama--far-p cur job))
      (ecoin-llama--abort cur)))
  (when ecoin-llama--queued
    (setf (ecoin-llama--job-done ecoin-llama--queued) t
          ecoin-llama--queued nil))
  (if ecoin-llama--inflight
      (progn (setf (ecoin-llama--job-waited job) t)
             (setq ecoin-llama--queued job)
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
   ((or (ecoin-llama--job-done job)
        (and (null (ecoin-llama--job-callback job))
             (not (ecoin-llama--background-job-p job))))
    (setf (ecoin-llama--job-done job) t))
   ((and (ecoin-llama--background-job-p job)
         (not (ecoin-llama--background-ok-p)))
    (setf (ecoin-llama--job-done job) t))
   ;; What it waited for may have answered it (a prefetch usually does).
   ((and (ecoin-llama--job-waited job) (not (ecoin-llama--job-manual job))
         (ecoin-llama--serve-from-cache job))
    (ecoin-llama--finish job))
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

(defun ecoin-llama--http-failure (status body &optional job)
  "Update the state for an unsuccessful STATUS with parsed BODY of JOB's request."
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
       ;; Only extra context can be shrunk; the prefix and suffix are capped.
       (when (and job (> (length (ecoin-llama--job-extra job)) 0))
         (cl-incf ecoin-llama--halvings))
       (if (> ecoin-llama--halvings 0)
           (ecoin-llama--note-once
            'context-halved
            "ecoin: llama server ran out of context; halving the extra context")
         (ecoin-llama--note-once
          'context "ecoin: llama server ran out of context; no completion")))
      (_ (ecoin-llama--set-state
          'error (format "ecoin: llama server answered %d%s" status
                         (if (string-empty-p msg) "" (concat ": " msg))))))))

(defun ecoin-llama--down ()
  "Record that the server cannot be reached."
  (ecoin-llama--set-state
   'down (format "ecoin: llama server unreachable at %s" ecoin-llama-url)))

(defun ecoin-llama--props-path ()
  "The path to probe: /props, for the configured model if there is one."
  (if-let* ((model (and (stringp ecoin-llama-model) ecoin-llama-model)))
      (concat ecoin-llama--path-props
              (format ecoin-llama--props-query (url-hexify-string model)))
    ecoin-llama--path-props))

;; These match llama.cpp's router texts, which may change between versions.
(defconst ecoin-llama--router-not-loaded "model is not loaded")
(defconst ecoin-llama--router-not-found-regexp "\\`model '.*' not found\\'")
(defconst ecoin-llama--router-role "router")

(defun ecoin-llama--probe-router-answer (status body)
  "Classify a router's 400 for the set model: `not-loaded', `not-found' or nil."
  (when (and (eql status 400) (stringp ecoin-llama-model))
    (let ((msg (ecoin-llama--error-message body)))
      (cond ((equal msg ecoin-llama--router-not-loaded) 'not-loaded)
            ((string-match-p ecoin-llama--router-not-found-regexp msg)
             'not-found)))))

(defun ecoin-llama--probe (job next)
  "GET /props for JOB, then call NEXT with JOB if the server answers."
  (setf (ecoin-llama--job-conn job)
        (ecoin-llama--http
         (ecoin-llama--job-target job) "GET" (ecoin-llama--props-path) nil
         ecoin-llama-request-timeout
         (lambda (result)
           (setf (ecoin-llama--job-conn job) nil)
           (let* ((status (plist-get result :status))
                  (router (ecoin-llama--probe-router-answer
                           status (plist-get result :body))))
             (cond
              ((null status) (ecoin-llama--down) (ecoin-llama--finish job))
              ((eq router 'not-loaded)
               (ecoin-llama--set-state
                'sleeping (format "ecoin: loading %s on the llama server..."
                                  ecoin-llama-model))
               ;; Only a user's request may make the router load the model.
               (if (ecoin-llama--job-callback job)
                   (funcall next job)
                 (ecoin-llama--finish job)))
              ((eq router 'not-found)
               (ecoin-llama--set-state
                'error (format "ecoin: llama server has no model %s; check `ecoin-llama-model'"
                               ecoin-llama-model))
               (ecoin-llama--finish job))
              ((/= status 200)
               (ecoin-llama--http-failure status (plist-get result :body))
               (ecoin-llama--finish job))
              ((and (null ecoin-llama-model)
                    (equal (plist-get (plist-get result :body) :role)
                           ecoin-llama--router-role))
               (ecoin-llama--set-state
                'error (format "ecoin: %s is a llama.cpp router; set `ecoin-llama-model'"
                               ecoin-llama-url))
               (ecoin-llama--finish job))
              (t
               (let ((props (plist-get result :body)))
                 (setq ecoin-llama--props props)
                 (ecoin-llama--set-state
                  (if (plist-get props :is_sleeping) 'sleeping 'ready)))
               ;; A background request must not wake a server that went to sleep.
               (if (or (ecoin-llama--job-callback job)
                       (and (ecoin-llama--background-job-p job)
                            (eq ecoin-llama--state 'ready)))
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

(defun ecoin-llama--model-field ()
  "The request-body fields that name the model; the setting is the only source."
  (when (stringp ecoin-llama-model) (list :model ecoin-llama-model)))

(defun ecoin-llama--warmup-body (job)
  "The /infill plist of the warm-up JOB: only the extra context, no generation."
  (append (list :input_prefix "" :input_suffix "" :prompt ""
                :input_extra (or (ecoin-llama--job-extra job) [])
                :n_predict 0 :samplers [] :cache_prompt t
                :t_max_predict_ms 1 :response_fields [""])
          (ecoin-llama--model-field)
          (when ecoin-llama-slot (list :id_slot ecoin-llama-slot))))

(defun ecoin-llama--request-body (job)
  "Build the /infill request plist for JOB."
  (if (ecoin-llama--job-warmup job)
      (ecoin-llama--warmup-body job)
    (ecoin-llama--completion-body job)))

(defun ecoin-llama--completion-body (job)
  "Build the /infill request plist of the completion or prefetch JOB."
  (let* ((ctx (ecoin-llama--job-ctx job))
         (n (ecoin-llama--alternatives job))
         (body (append
                (list :input_prefix (plist-get ctx :prefix)
                      :input_suffix (plist-get ctx :suffix)
                      :prompt (plist-get ctx :middle)
                      :input_extra (or (ecoin-llama--job-extra job) [])
                      :n_predict ecoin-llama-n-predict
                      :n_indent (plist-get ctx :n-indent)
                      :t_max_predict_ms
                      (if (ecoin-llama--job-prefetch job)
                          ecoin-llama-prefetch-t-max-predict-ms
                        ecoin-llama-t-max-predict-ms)
                      :stream :false
                      :cache_prompt t
                      :response_fields ecoin-llama--response-fields)
                (ecoin-llama--model-field)
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
  (let ((wake (eq ecoin-llama--state 'sleeping))
        (extra (ecoin-llama--ring-extra (ecoin-llama--job-ring job))))
    ;; The key of the answer is the extra context that was really sent.
    (setf (ecoin-llama--job-extra job) (car extra)
          (ecoin-llama--job-sent job) (float-time)
          ecoin-llama--warmed (cdr extra))
    (when (ecoin-llama--job-ctx job)
      (setf (ecoin-llama--job-ctx job)
            (ecoin-llama--ctx-with-id (ecoin-llama--job-ctx job) (cdr extra))))
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
      (if (ecoin-llama--job-warmup job)
          (ecoin-llama--note-warmup job)
        (ecoin-llama--deliver job (plist-get result :body)))
      (ecoin-llama--finish job))
     (t (ecoin-llama--http-failure status (plist-get result :body) job)
        (ecoin-llama--finish job)))))

(defun ecoin-llama--after-timeout (job)
  "The server answered /props after JOB's request timed out."
  (if (and (eq ecoin-llama--state 'sleeping)
           (not (ecoin-llama--job-waking job))
           (not (ecoin-llama--background-job-p job)))
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

(defun ecoin-llama--stat-count (key)
  "Count one KEY for `ecoin-stats'; never signals."
  (condition-case nil
      (cl-incf (alist-get key ecoin-llama--stat-counts 0))
    (error nil)))

(defun ecoin-llama--stat-delivered (job)
  "Record the completion JOB just received; never signals."
  (condition-case nil
      (cond ((ecoin-llama--job-prefetch job) (ecoin-llama--stat-count 'prefetch))
            (t (unless (ecoin-llama--job-manual job)
                 (ecoin-llama--stat-count 'miss))
               (ecoin--ring-push ecoin-llama--stat-timings
                                 (copy-sequence ecoin-llama--last-timings))))
    (error nil)))

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
    (ecoin-llama--stat-delivered job)
    ;; Whatever happened to the context meanwhile, the answer is still good.
    ;; A blank answer to a guess must not become a zero-request "no ghost"
    ;; hit when the user really gets there.
    (ecoin-llama--cache-store
     (ecoin-llama--job-ctx job)
     (if (ecoin-llama--job-prefetch job)
         (seq-filter (lambda (c) (string-match-p "[^ \t\n]" c))
                     (ecoin-llama--contents body))
       (ecoin-llama--contents body)))
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

(defun ecoin-llama--context (&optional insertion)
  "Return the /infill context around point as a plist.
With INSERTION, a string, build it as if INSERTION were inserted at point and
point were after it; the buffer is not changed.
Keys: :prefix :middle :suffix :n-indent :text-before :text-after."
  (save-restriction
    (widen)
    (save-excursion
      (let* ((inhibit-field-text-motion t)
             (pt (point))
             (bol (progn (forward-line 0) (point)))
             (eol (line-end-position))
             (head (concat (buffer-substring-no-properties bol pt) insertion))
             ;; Complete lines of the insertion join the lines above; the
             ;; last one is the line point is on.
             (parts (split-string head "\n"))
             (text-before (car (last parts)))
             (extra (last (butlast parts) ecoin-llama-n-prefix))
             (text-after (buffer-substring-no-properties pt eol))
             (line (concat text-before text-after))
             (blank (string-match-p "\\`[ \t]*\\'" line))
             (above (concat
                     (buffer-substring-no-properties
                      (progn (forward-line (- (max 0 (- ecoin-llama-n-prefix
                                                        (length extra)))))
                             (point))
                      bol)
                     (mapconcat (lambda (l) (concat l "\n")) extra "")))
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

(defconst ecoin-llama--bracket-pairs '((?\( . ?\)) (?\[ . ?\]) (?{ . ?}))
  "Opener and closer chars checked for balance on a line.")
(defconst ecoin-llama--quote-chars '(?\" ?\' ?`)
  "Quote chars checked by parity on a line.")

(defun ecoin-llama--char-balanced-p (str char)
  "Non-nil if CHAR is balanced in STR: a quote by parity, a closer by depth."
  (if-let* ((open (car (rassq char ecoin-llama--bracket-pairs))))
      (let ((depth 0))
        (seq-doseq (c str)
          (cond ((eq c open) (setq depth (1+ depth)))
                ((eq c char) (setq depth (1- depth))))
          ;; A closer without opener stays unbalanced whatever follows.
          (when (< depth 0) (setq depth most-negative-fixnum)))
        (= depth 0))
    (cl-evenp (seq-count (lambda (c) (eq c char)) str))))

(defun ecoin-llama--trim-unbalances-p (before first after tail)
  "Non-nil if dropping TAIL from FIRST would unbalance a closer or quote of TAIL.
That is, the line is balanced with FIRST and AFTER but not without TAIL,
as when FIRST is a(b) and AFTER is the auto-paired closer."
  (let ((trimmed (substring first 0 (- (length first) (length tail)))))
    (cl-some (lambda (c)
               (and (or (memq c ecoin-llama--quote-chars)
                        (rassq c ecoin-llama--bracket-pairs))
                    (ecoin-llama--char-balanced-p (concat before first after) c)
                    (not (ecoin-llama--char-balanced-p
                          (concat before trimmed after) c))))
             (seq-uniq (string-to-list tail)))))

(defun ecoin-llama--replaces-closers-p (before ghost after)
  "Non-nil if GHOST already carries the closers of AFTER that auto-pairing added.
That is, BEFORE+GHOST+AFTER is unbalanced for each closer or quote in AFTER
while BEFORE+GHOST is balanced.  AFTER must hold nothing but closers."
  (let ((chars (seq-filter (lambda (c) (or (memq c ecoin-llama--quote-chars)
                                           (rassq c ecoin-llama--bracket-pairs)))
                           (seq-uniq (string-to-list after)))))
    (and chars
         (string-match-p ecoin-llama-line-suffix-regexp after)
         (cl-every (lambda (c)
                     (and (ecoin-llama--char-balanced-p (concat before ghost) c)
                          (not (ecoin-llama--char-balanced-p
                                (concat before ghost after) c))))
                   chars))))

(defun ecoin-llama--postprocess (content &optional keep-indent)
  "Turn the raw server CONTENT into ghost text for point, or nil for none.
See `ecoin-llama--postprocess-replace' for KEEP-INDENT."
  (car (ecoin-llama--postprocess-replace content keep-indent)))

(defun ecoin-llama--postprocess-replace (content &optional keep-indent)
  "Post-process CONTENT into (TEXT . REPLACE-EOL), or nil for no ghost.
REPLACE-EOL is non-nil when TEXT carries the closers after point, which
accepting must replace.
With KEEP-INDENT, put back the indentation removed from a whitespace-only
line, since the core's dedent step on delivery removes it."
  (let* ((text (replace-regexp-in-string ecoin-llama--leak-regexp "" content t t))
         (rev (reverse (split-string text "\n")))
         (text-before (buffer-substring-no-properties
                       (line-beginning-position) (point)))
         (text-after (buffer-substring-no-properties
                       (point) (line-end-position)))
         (blank-line (string-blank-p (concat text-before text-after)))
         (removed "")
         (replace nil))
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
                (when (and (> (length tail) 0) (string-suffix-p tail first)
                           (not (ecoin-llama--trim-unbalances-p
                                 text-before first text-after tail)))
                  (setq first (substring first 0 (- (length first)
                                                    (length tail)))))
                (when (and (equal first (car lines))
                           (ecoin-llama--replaces-closers-p
                            text-before first text-after))
                  (setq replace t))
                (setq lines (list first))))
            (when (cdr lines)
              (setq lines (ecoin-llama--snip-closers lines next)))
            (let ((out (string-join lines "\n")))
              (when (string-match-p "[^ \t\n]" out)
                (cons (concat (and keep-indent removed) out) replace)))))))))

(defun ecoin-llama--items (contents)
  "Convert raw CONTENTS to `ecoin-item's for point; drop empties and duplicates."
  (let ((seen nil) (items nil))
    (dolist (c contents)
      (when-let* ((res (ecoin-llama--postprocess-replace c t)))
        (let ((key (string-trim (car res))))
          (unless (member key seen)
            (push key seen)
            (push (ecoin-make-item :text (car res) :backend 'llama
                                   :end (and (cdr res) (line-end-position)))
                  items)))))
    (nreverse items)))

;;;; Cache

(defconst ecoin-llama--cache-separator "\x1e"
  "Between the text before point and the suffix in a cache key.")
(defconst ecoin-llama--cache-max-contents 3
  "Raw completions kept per key.")
(defconst ecoin-llama--cache-shifted-keys 3
  "Keys stored per answer with the first 1..N prefix lines dropped.")
(defconst ecoin-llama--typed-back-max 128
  "Characters before point the typed-through lookup looks back over.")

(defvar ecoin-llama--cache (make-hash-table :test #'equal)
  "Key (sha256 of a context) to a list of raw /infill contents, newest first.")
(defvar ecoin-llama--cache-order nil
  "Cache keys, most recently used first.")

(defun ecoin-llama--cache-clear ()
  "Forget every cached completion."
  (clrhash ecoin-llama--cache)
  (setq ecoin-llama--cache-order nil))

(defconst ecoin-llama--extra-separator "\x1d"
  "Between the extra-context id and the rest of a cache key.")

(defun ecoin-llama--cache-key (before suffix &optional extra-id)
  "Key of the text BEFORE point (prefix and middle) with SUFFIX after it.
EXTRA-ID identifies the extra context the answer was computed with, since
the server's prompt starts with it; nil (no extra) gives the plain key."
  (secure-hash 'sha256 (encode-coding-string
                        (concat (and extra-id
                                     (concat extra-id ecoin-llama--extra-separator))
                                before ecoin-llama--cache-separator suffix)
                        'utf-8 t)))

(defun ecoin-llama--cache-touch (key)
  "Make KEY the most recently used."
  (setq ecoin-llama--cache-order
        (cons key (delete key ecoin-llama--cache-order))))

(defun ecoin-llama--cache-put (key contents)
  "Add CONTENTS, raw strings, under KEY and evict the least recently used."
  (puthash key (seq-take (seq-uniq (append contents (gethash key ecoin-llama--cache)))
                         ecoin-llama--cache-max-contents)
           ecoin-llama--cache)
  (ecoin-llama--cache-touch key)
  (while (> (length ecoin-llama--cache-order) (max 1 ecoin-llama-cache-size))
    (remhash (car (last ecoin-llama--cache-order)) ecoin-llama--cache)
    (setq ecoin-llama--cache-order (butlast ecoin-llama--cache-order))))

(defun ecoin-llama--cache-store (ctx contents)
  "Cache the raw CONTENTS answering the context CTX.
Also store them under the keys with the first 1..3 prefix lines dropped, so
a hit survives the prefix window shifting after a newline."
  (let ((prefix (plist-get ctx :prefix))
        (middle (plist-get ctx :middle))
        (suffix (plist-get ctx :suffix))
        (id (plist-get ctx :extra-id))
        (n 0))
    (when contents
      (while prefix
        (ecoin-llama--cache-put
         (ecoin-llama--cache-key (concat prefix middle) suffix id) contents)
        (let ((nl (and (< n ecoin-llama--cache-shifted-keys)
                       (string-search "\n" prefix))))
          (setq prefix (and nl (substring prefix (1+ nl)))
                n (1+ n)))))))

(defun ecoin-llama--cache-has-p (ctx)
  "Non-nil if the exact context CTX is cached."
  (gethash (ecoin-llama--cache-key (concat (plist-get ctx :prefix)
                                           (plist-get ctx :middle))
                                   (plist-get ctx :suffix)
                                   (plist-get ctx :extra-id))
           ecoin-llama--cache))

(defun ecoin-llama--cache-typed-through (before suffix &optional extra-id)
  "Items for text BEFORE point (prefix and middle) from an older shorter context.
SUFFIX follows point; only answers computed with the same EXTRA-ID count.
Return the post-processed items of the longest cached remainder that is
still a ghost, or nil.  The chars typed since are removed from the front."
  (let ((n (length before)) (candidates nil))
    (cl-loop for i from 1 to (min ecoin-llama--typed-back-max n)
             for key = (ecoin-llama--cache-key (substring before 0 (- n i))
                                               suffix extra-id)
             for hit = (gethash key ecoin-llama--cache)
             for typed = (and hit (substring before (- n i)))
             do (dolist (c hit)
                  (when (and (string-prefix-p typed c) (> (length c) i))
                    (push (cons (substring c i) key) candidates))))
    (setq candidates (sort candidates (lambda (a b) (> (length (car a))
                                                       (length (car b))))))
    (cl-loop for (rest . key) in candidates
             for items = (ecoin-llama--items (list rest))
             when items do (ecoin-llama--cache-touch key) and return items)))

(defun ecoin-llama--cache-lookup (ctx)
  "Look up the context CTX at point.  Return (ITEMS) on a hit, else nil.
Exact key first, then typed-through reuse."
  (let* ((before (concat (plist-get ctx :prefix) (plist-get ctx :middle)))
         (suffix (plist-get ctx :suffix))
         (id (plist-get ctx :extra-id))
         (key (ecoin-llama--cache-key before suffix id)))
    (if-let* ((hit (gethash key ecoin-llama--cache)))
        (progn (ecoin-llama--cache-touch key)
               (ecoin-llama--stat-count 'exact)
               (list (ecoin-llama--items hit)))
      (when (> (hash-table-count ecoin-llama--cache) 0)
        (when-let* ((items (ecoin-llama--cache-typed-through before suffix id)))
          (ecoin-llama--stat-count 'typed)
          (list items))))))

(defun ecoin-llama--call-with-items (callback items)
  "Call CALLBACK with ITEMS; log instead of signalling."
  (condition-case err
      (funcall callback items)
    (error (ecoin--log "deliver: %s" (car err)))))

(defun ecoin-llama--serve-from-cache (job)
  "Answer JOB from the cache if its buffer is unchanged; non-nil if it did."
  (let ((buffer (ecoin-llama--job-buffer job)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (= (point) (ecoin-llama--job-point job))
                   (= (buffer-chars-modified-tick) (ecoin-llama--job-tick job)))
          (when-let* ((hit (ecoin-llama--cache-lookup (ecoin-llama--job-ctx job))))
            (ecoin-llama--call-with-items (ecoin-llama--job-callback job)
                                          (car hit))
            t))))))

;;;; Background requests

(defun ecoin-llama--note-activity ()
  "Record a user trigger."
  (setq ecoin-llama--last-activity (float-time)))

(defun ecoin-llama--background-ok-p ()
  "Non-nil if a request nobody asked for may be sent now.
The server must be ready, and the user must have triggered recently."
  (and (eq ecoin-llama--state 'ready)
       (not (ecoin-llama--in-backoff-p))
       ecoin-llama--last-activity
       (< (- (float-time) ecoin-llama--last-activity)
          ecoin-llama-activity-window)))

(defun ecoin-llama--prefetch-fire (buffer ctx)
  "Send the prefetch for CTX if BUFFER still shows the ghost it was made for.
Typing into the ghost keeps CTX valid; anything else makes it useless, and
it would only hold the single flight."
  (setq ecoin-llama--prefetch-timer nil)
  (when (and (buffer-live-p buffer)
             (not ecoin-llama--inflight)
             (not ecoin-llama--queued)
             (ecoin-llama--background-ok-p)
             (not (ecoin-llama--cache-has-p ctx)))
    (with-current-buffer buffer
      (when (and (ecoin--visible-p)
                 ;; The ring may have changed since; send refreshes the id.
                 (equal (ecoin-llama--ctx-with-id ctx nil)
                        (ecoin-llama--ctx-with-id
                         (ecoin-llama--context (ecoin--overlay-ghost)) nil)))
        (when-let* ((target (condition-case nil (ecoin-llama--target)
                              (user-error nil))))
          (ecoin-llama--start
           (ecoin-llama--make-job :buffer buffer :point (point)
                                  :tick (buffer-chars-modified-tick)
                                  :ctx ctx :target target :prefetch t
                                  :ring (ecoin-llama--current-ring))))))))

(defun ecoin-llama--note-shown (item)
  "A ghost for ITEM is displayed: cache what accepting its first line leaves.
If prefetching is on, also schedule the request for the context after
accepting all of it."
  (let ((ghost (ecoin-item-text item)))
    (when (and (null (ecoin-item-end item))
               (> (length ghost) 0)
               (string-match "\\`\n*[^\n]*" ghost))
      (let ((line (match-string 0 ghost))
            (rest (substring ghost (match-end 0))))
        (when (> (length rest) 0)
          (ecoin-llama--cache-store
           (ecoin-llama--context-for (ecoin-llama--current-ring) line)
           (list rest))))
      (when (and ecoin-llama-prefetch
                 (ecoin-llama--background-ok-p)
                 (string-match-p ecoin-llama-line-suffix-regexp
                                 (buffer-substring-no-properties
                                  (point) (line-end-position))))
        (let ((ctx (ecoin-llama--context-for (ecoin-llama--current-ring) ghost)))
          (unless (ecoin-llama--cache-has-p ctx)
            (when ecoin-llama--prefetch-timer
              (cancel-timer ecoin-llama--prefetch-timer))
            ;; Not now: the hook runs inside the delivery of the request
            ;; that is still counted as in flight.
            (setq ecoin-llama--prefetch-timer
                  (run-with-timer 0 nil #'ecoin-llama--prefetch-fire
                                  (current-buffer) ctx))))))))

;;;; Extra-context ring

;; Every project (its root, or the directory of a buffer outside any project)
;; has a ring of recently seen code chunks.  They are sent as `input_extra',
;; which llama-server puts first in the prompt, so a warm-up request keeps
;; them in its KV cache and a completion only processes the local context.
;; Chunks come from file buffers with `ecoin-mode' on that are not excluded
;; (`ecoin--excluded-p'), and only ever go to the ring of their own project.

(declare-function project-current "project")
(declare-function project-root "project")

(defconst ecoin-llama--queue-max 16 "Chunks waiting to enter a ring.")
(defconst ecoin-llama--pick-min-lines 3 "Fewest lines of a chunk.")
(defconst ecoin-llama--far-distance 32
  "Lines point must move after the last far pick for the next one.")
(defconst ecoin-llama--far-scope 1024 "How far above point far picks look.")
(defconst ecoin-llama--far-suffix-span 64 "Lines of the far window below point.")
(defconst ecoin-llama--evict-on-pick 0.9
  "Similarity above which a new chunk evicts an old one (far picks: is dropped).")
(defconst ecoin-llama--evict-at-request 0.5
  "Similarity to the text around point above which a chunk is evicted.")
(defconst ecoin-llama--token-regexp "[^[:alnum:]_]+"
  "Separates tokens; not `\\W', which follows the buffer's syntax table.")
(defconst ecoin-llama--kill-commands
  '(kill-ring-save kill-region evil-yank evil-delete)
  "Commands whose result in `kill-ring' may become a chunk.")

(cl-defstruct (ecoin-llama--ring (:constructor ecoin-llama--make-ring))
  "Extra context of one project.  CHUNKS and QUEUE are oldest first.
Each chunk is (:filename REL :text STR :time T).  VERSION counts changes of
CHUNKS; MEMO caches the serialized extra; FAR marks the last far pick."
  chunks queue (version 0) memo far)

(defvar-local ecoin-llama--root-cache nil "(DIRECTORY . ROOT) of this buffer.")
(defvar ecoin-llama--chunk-tokens (make-hash-table :test #'eq :weakness 'key)
  "Chunk to the set of its tokens.")

;;;;; Similarity

(defun ecoin-llama--token-set (text)
  "Hash set of the tokens of TEXT."
  (let ((set (make-hash-table :test #'equal)))
    (dolist (token (split-string text ecoin-llama--token-regexp t))
      (puthash token t set))
    set))

(defun ecoin-llama--dice (set0 set1)
  "Dice coefficient 2|common| / (|SET0| + |SET1|) of two token sets.
This is the set variant: llama.vim counts duplicate tokens of one text
against the other's set, which can exceed 1.  Two empty sets are equal."
  (let ((n0 (hash-table-count set0))
        (n1 (hash-table-count set1))
        (common 0))
    (if (= 0 n0 n1)
        1.0
      (let ((small (if (<= n0 n1) set0 set1))
            (large (if (<= n0 n1) set1 set0)))
        (maphash (lambda (token _) (when (gethash token large) (cl-incf common)))
                 small)
        (/ (* 2.0 common) (+ n0 n1))))))

(defun ecoin-llama--similarity (text0 text1)
  "Dice similarity of the tokens of TEXT0 and TEXT1."
  (ecoin-llama--dice (ecoin-llama--token-set text0)
                     (ecoin-llama--token-set text1)))

(defun ecoin-llama--chunk-tokens (chunk)
  "The token set of CHUNK, computed once."
  (or (gethash chunk ecoin-llama--chunk-tokens)
      (puthash chunk (ecoin-llama--token-set (plist-get chunk :text))
               ecoin-llama--chunk-tokens)))

;;;;; Rings

(defun ecoin-llama--project-root ()
  "Root of the current buffer's project, or nil."
  (when (require 'project nil t)
    (condition-case nil
        (when-let* ((project (project-current)))
          (file-name-as-directory (expand-file-name (project-root project))))
      (error nil))))

(defun ecoin-llama--root ()
  "Key of the current buffer's ring, cached; nil for remote directories."
  (unless (file-remote-p default-directory)
    (let ((dir default-directory))
      (if (equal (car ecoin-llama--root-cache) dir)
          (cdr ecoin-llama--root-cache)
        (let ((root (or (ecoin-llama--project-root)
                        (file-name-as-directory (expand-file-name dir)))))
          (setq ecoin-llama--root-cache (cons dir root))
          root)))))

(defun ecoin-llama--current-ring (&optional create)
  "The ring of the current buffer's project; CREATE it if missing.
Nil when the ring is off."
  (when (> ecoin-llama-ring-chunks 0)
    (when-let* ((root (ecoin-llama--root)))
      (or (gethash root ecoin-llama--rings)
          (and create
               (puthash root (ecoin-llama--make-ring) ecoin-llama--rings))))))

(defun ecoin-llama--source-p ()
  "Non-nil if the current buffer may contribute chunks."
  (and (eq ecoin-backend 'llama)
       (> ecoin-llama-ring-chunks 0)
       buffer-file-name
       (bound-and-true-p ecoin-mode)
       (not (file-remote-p buffer-file-name))
       (not (ecoin--excluded-p))))

(defun ecoin-llama--relative-name ()
  "File name of the current buffer relative to its ring root, or nil if outside."
  (when-let* ((root (ecoin-llama--root)))
    (let ((rel (file-relative-name buffer-file-name root)))
      (unless (string-prefix-p "../" rel) rel))))

;;;;; Extra text: serialization and its id

(defun ecoin-llama--extra-cap ()
  "Effective `ecoin-llama-extra-max-chars' after the halvings of this session."
  (ash ecoin-llama-extra-max-chars (- ecoin-llama--halvings)))

(defun ecoin-llama--serialize (chunks)
  "Vector of the objects sent for CHUNKS, oldest first, under the char cap.
The newest chunks win: older ones are dropped until the text fits."
  (let ((budget (ecoin-llama--extra-cap))
        (kept nil))
    (cl-loop for chunk in (reverse (last chunks (max 0 ecoin-llama-ring-chunks)))
             do (setq budget (- budget (length (plist-get chunk :text))))
             while (>= budget 0)
             do (push (list :filename (plist-get chunk :filename)
                            :text (plist-get chunk :text))
                      kept))
    (vconcat kept)))

(defun ecoin-llama--ring-extra (ring)
  "Return (VECTOR . ID) for RING: the `input_extra' to send and its identity.
ID is a hash of the exact serialized extra, nil when it is empty, so two
rings with the same text share cache entries and an empty ring changes none."
  (if (null ring)
      (cons [] nil)
    (let ((stamp (list (ecoin-llama--ring-version ring)
                       (ecoin-llama--extra-cap) ecoin-llama-ring-chunks)))
      (if (equal (car (ecoin-llama--ring-memo ring)) stamp)
          (cdr (ecoin-llama--ring-memo ring))
        (let* ((vector (ecoin-llama--serialize (ecoin-llama--ring-chunks ring)))
               (id (and (> (length vector) 0)
                        (secure-hash
                         'sha256
                         (encode-coding-string
                          (mapconcat (lambda (o) (concat (plist-get o :filename)
                                                         "\n" (plist-get o :text)))
                                     vector "\x1f")
                          'utf-8 t))))
               (result (cons vector id)))
          (setf (ecoin-llama--ring-memo ring) (cons stamp result))
          result)))))

(defun ecoin-llama--extra-chars (vector)
  "Characters of text in the extra VECTOR."
  (cl-loop for o across vector sum (length (plist-get o :text))))

(defun ecoin-llama--ctx-with-id (ctx id)
  "CTX with its :extra-id set to ID (removed when ID is nil)."
  (let ((rest (cl-loop for (key value) on ctx by #'cddr
                       unless (eq key :extra-id) append (list key value))))
    (if id (append rest (list :extra-id id)) rest)))

(defun ecoin-llama--context-for (ring &optional insertion)
  "`ecoin-llama--context' at point (see INSERTION there) plus RING's extra id."
  (ecoin-llama--ctx-with-id (ecoin-llama--context insertion)
                            (cdr (ecoin-llama--ring-extra ring))))

;;;;; Picks

(defun ecoin-llama--with-newline (text)
  "TEXT ending in a newline."
  (if (string-suffix-p "\n" text) text (concat text "\n")))

(defun ecoin-llama--lines-around (pos half)
  "Text of the lines HALF above to HALF below the line at POS, widened."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char pos)
      (forward-line 0)
      (let* ((bol (point))
             (start (progn (forward-line (- half)) (point)))
             (end (progn (goto-char bol) (forward-line (1+ half)) (point))))
        (ecoin-llama--with-newline (buffer-substring-no-properties start end))))))

(defun ecoin-llama--random-offset (max)
  "A random integer from 0 to MAX; tests replace it."
  (random (1+ max)))

(defun ecoin-llama--remove-chunks (ring similar)
  "Drop the chunks SIMILAR from RING's queue and chunks."
  (when similar
    (setf (ecoin-llama--ring-queue ring)
          (seq-remove (lambda (c) (memq c similar)) (ecoin-llama--ring-queue ring)))
    (when (seq-some (lambda (c) (memq c similar)) (ecoin-llama--ring-chunks ring))
      (setf (ecoin-llama--ring-chunks ring)
            (seq-remove (lambda (c) (memq c similar)) (ecoin-llama--ring-chunks ring)))
      (cl-incf (ecoin-llama--ring-version ring)))))

(defun ecoin-llama--enqueue (ring chunk evict)
  "Queue CHUNK in RING; non-nil if it was queued.
A chunk identical to a known one is dropped.  Known chunks more similar to
it than `ecoin-llama--evict-on-pick' are evicted when EVICT, else CHUNK is
dropped."
  (let* ((known (append (ecoin-llama--ring-queue ring) (ecoin-llama--ring-chunks ring)))
         (tokens (ecoin-llama--token-set (plist-get chunk :text))))
    (unless (seq-some (lambda (c) (and (equal (plist-get c :text) (plist-get chunk :text))
                                       (equal (plist-get c :filename)
                                              (plist-get chunk :filename))))
                      known)
      (let ((similar (seq-filter (lambda (c) (> (ecoin-llama--dice
                                                 (ecoin-llama--chunk-tokens c) tokens)
                                                ecoin-llama--evict-on-pick))
                                 known)))
        (when (or evict (null similar))
          (puthash chunk tokens ecoin-llama--chunk-tokens)
          (ecoin-llama--remove-chunks ring similar)
          (setf (ecoin-llama--ring-queue ring)
                (last (append (ecoin-llama--ring-queue ring) (list chunk))
                      ecoin-llama--queue-max))
          t)))))

(defun ecoin-llama--pick (ring name text evict)
  "Queue TEXT, from the file NAME, in RING as a chunk."
  (when (and (>= (cl-count ?\n text) ecoin-llama--pick-min-lines)
             (string-match-p "[[:alnum:]_]" text)
             (ecoin-llama--enqueue
              ring (list :filename name :text text :time (float-time)) evict))
    (ecoin--log "ring: picked %d lines; %d queued, %d in the ring"
                (cl-count ?\n text)
                (length (ecoin-llama--ring-queue ring))
                (length (ecoin-llama--ring-chunks ring)))))

(defun ecoin-llama--point-of (buffer)
  "Point of BUFFER as the user sees it."
  (if-let* ((window (get-buffer-window buffer)))
      (window-point window)
    (with-current-buffer buffer (point))))

(defun ecoin-llama--pick-around-point (buffer &optional modified-ok)
  "Queue the lines around point of BUFFER, an unmodified source file.
MODIFIED-OK allows a buffer with unsaved changes."
  (when (buffer-live-p buffer)
    (let ((pos (ecoin-llama--point-of buffer)))
      (with-current-buffer buffer
        (when (and (or modified-ok (not (buffer-modified-p)))
                   (ecoin-llama--source-p))
          (when-let* ((ring (ecoin-llama--current-ring t))
                      (name (ecoin-llama--relative-name)))
            (ecoin-llama--pick
             ring name
             (ecoin-llama--lines-around pos (/ ecoin-llama-ring-chunk-lines 2))
             t)))))))

(defun ecoin-llama--yank-text (text)
  "TEXT cut to at most `ecoin-llama-ring-chunk-lines' lines, from a random start."
  (let* ((lines (split-string (string-remove-suffix "\n" text) "\n"))
         (size (max 1 ecoin-llama-ring-chunk-lines))
         (n (length lines)))
    (if (<= n size)
        (ecoin-llama--with-newline text)
      (let ((from (ecoin-llama--random-offset (- n size))))
        (concat (mapconcat #'identity (seq-subseq lines from (+ from size)) "\n")
                "\n")))))

(defun ecoin-llama--window-text (start lines)
  "A random `ecoin-llama-ring-chunk-lines' window of the LINES lines at START."
  (let* ((size (max 1 ecoin-llama-ring-chunk-lines))
         (offset (if (> lines size)
                     (ecoin-llama--random-offset (- lines size))
                   0)))
    (goto-char start)
    (forward-line offset)
    (let ((from (point)))
      (forward-line (min size lines))
      (ecoin-llama--with-newline (buffer-substring-no-properties from (point))))))

(defun ecoin-llama--far-texts ()
  "Random windows far above and below point, as a list of texts."
  (save-restriction
    (widen)
    (save-excursion
      (forward-line 0)
      (let ((bol (point)) (texts nil))
        ;; Lines [y - scope, y - n_prefix]: just above what the prompt holds.
        (when (zerop (forward-line (- ecoin-llama-n-prefix)))
          (let* ((above (max 0 (- ecoin-llama--far-scope ecoin-llama-n-prefix)))
                 (short (abs (forward-line (- above))))
                 (lines (- (1+ above) short)))
            (when (>= lines ecoin-llama--pick-min-lines)
              (push (ecoin-llama--window-text (point) lines) texts))))
        ;; Lines [y + n_suffix, y + n_suffix + 64].
        (goto-char bol)
        (when (zerop (forward-line ecoin-llama-n-suffix))
          (let* ((start (point))
                 (short (forward-line (1+ ecoin-llama--far-suffix-span)))
                 (lines (- (1+ ecoin-llama--far-suffix-span) short)))
            (when (>= lines ecoin-llama--pick-min-lines)
              (push (ecoin-llama--window-text start lines) texts))))
        (nreverse texts)))))

(defun ecoin-llama--lines-apart-p (a b)
  "Non-nil if positions A and B are over `ecoin-llama--far-distance' lines apart."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (min a b))
      (and (zerop (forward-line (1+ ecoin-llama--far-distance)))
           (<= (point) (max a b))))))

(defun ecoin-llama--maybe-far-pick (ring)
  "After a request: when point moved far since RING's last far pick, pick again.
Far chunks are skipped, not evicting, when similar to a known one."
  (when (and ring (ecoin-llama--source-p))
    (let ((mark (ecoin-llama--ring-far ring)))
      (when (or (not (and mark (eq (marker-buffer mark) (current-buffer))))
                (ecoin-llama--lines-apart-p (marker-position mark) (point)))
        (if mark
            (set-marker mark (point))
          (setf (ecoin-llama--ring-far ring) (point-marker)))
        (when-let* ((name (ecoin-llama--relative-name)))
          (dolist (text (ecoin-llama--far-texts))
            (ecoin-llama--pick ring name text nil)))))))

;;;;; Hooks: buffer switches, saves, copies

(defun ecoin-llama--note-switch (buffer)
  "BUFFER is now selected: pick around point in the buffer left and in BUFFER."
  (let ((prev ecoin-llama--prev-buffer))
    (unless (or (eq buffer prev) (not (buffer-live-p buffer))
                (with-current-buffer buffer (minibufferp)))
      (setq ecoin-llama--prev-buffer buffer)
      (ecoin-llama--pick-around-point prev)
      (ecoin-llama--pick-around-point buffer))))

(defun ecoin-llama--switch-fire ()
  "Run the picks of a window change, outside redisplay."
  (setq ecoin-llama--switch-timer nil)
  (ecoin--hook #'ecoin-llama--note-switch (window-buffer (selected-window))))

(defun ecoin-llama--on-window-change (&rest _)
  "Hook: defer the picks, which may look up the project, out of redisplay."
  (when (and (eq ecoin-backend 'llama) (> ecoin-llama-ring-chunks 0)
             (not ecoin-llama--switch-timer))
    (setq ecoin-llama--switch-timer
          (run-with-timer 0 nil #'ecoin-llama--switch-fire))))

(defun ecoin-llama--on-save ()
  "Hook: pick around point of the buffer just saved."
  (when (bound-and-true-p ecoin-mode)
    (ecoin--hook #'ecoin-llama--pick-around-point (current-buffer) t)))

(defun ecoin-llama--on-command ()
  "Hook: remember which buffer a copy or kill came from, if it may be used."
  (when (and (memq this-command ecoin-llama--kill-commands)
             (stringp (car kill-ring)))
    (setq ecoin-llama--kill-source
          (and (ignore-errors (ecoin-llama--source-p))
               (cons (current-buffer) (car kill-ring))))))

(defun ecoin-llama--poll-kill ()
  "Pick from `kill-ring' when its head changed and the source is known."
  (let ((head (car kill-ring)))
    (when (and (stringp head) (not (eq head ecoin-llama--seen-kill)))
      (setq ecoin-llama--seen-kill head)
      (when-let* ((source ecoin-llama--kill-source)
                  ((eq (cdr source) head))
                  ((buffer-live-p (car source))))
        (setq ecoin-llama--kill-source nil)
        (with-current-buffer (car source)
          (when (ecoin-llama--source-p)
            (when-let* ((ring (ecoin-llama--current-ring t))
                        (name (ecoin-llama--relative-name)))
              (ecoin-llama--pick ring name (ecoin-llama--yank-text head) t))))))))

(add-hook 'after-save-hook #'ecoin-llama--on-save)
(add-hook 'post-command-hook #'ecoin-llama--on-command)
(add-hook 'window-buffer-change-functions #'ecoin-llama--on-window-change)
(add-hook 'window-selection-change-functions #'ecoin-llama--on-window-change)

(defun ecoin-llama-unload-function ()
  "Remove the hooks of the ring; called by `unload-feature'."
  (remove-hook 'after-save-hook #'ecoin-llama--on-save)
  (remove-hook 'post-command-hook #'ecoin-llama--on-command)
  (remove-hook 'window-buffer-change-functions #'ecoin-llama--on-window-change)
  (remove-hook 'window-selection-change-functions #'ecoin-llama--on-window-change)
  nil)

;;;;; Eviction at request time

(defun ecoin-llama--evict-local (ring buffer &optional with-queue)
  "Evict RING's chunks too similar to the text around point in BUFFER.
Chunks like that make the model repeat what is already there.  WITH-QUEUE
also evicts from the queue."
  (when (and ring (buffer-live-p buffer)
             (or (ecoin-llama--ring-chunks ring)
                 (and with-queue (ecoin-llama--ring-queue ring))))
    (let ((window (with-current-buffer buffer
                    (ecoin-llama--token-set
                     (ecoin-llama--lines-around
                      (point) (/ ecoin-llama-ring-chunk-lines 2))))))
      (ecoin-llama--remove-chunks
       ring
       (seq-filter (lambda (c) (> (ecoin-llama--dice (ecoin-llama--chunk-tokens c)
                                                     window)
                                  ecoin-llama--evict-at-request))
                   (append (and with-queue (ecoin-llama--ring-queue ring))
                           (ecoin-llama--ring-chunks ring)))))))

;;;;; Ring timer and warm-up

(defun ecoin-llama--ring-alive-p ()
  "Non-nil while the ring timer has work: a recent trigger, an ecoin buffer."
  (and (> ecoin-llama-ring-chunks 0)
       ecoin-llama--last-activity
       (< (- (float-time) ecoin-llama--last-activity) ecoin-llama-activity-window)
       (seq-some (lambda (b) (buffer-local-value 'ecoin-mode b)) (buffer-list))))

(defun ecoin-llama--ring-stop ()
  "Cancel the ring timer."
  (when ecoin-llama--ring-timer
    (cancel-timer ecoin-llama--ring-timer)
    (setq ecoin-llama--ring-timer nil)))

(defun ecoin-llama--ring-ensure-timer ()
  "Start the ring timer unless it runs or the ring has no work."
  (when (and (not ecoin-llama--ring-timer) (ecoin-llama--ring-alive-p))
    (setq ecoin-llama--ring-timer
          (run-with-timer ecoin-llama-ring-interval ecoin-llama-ring-interval
                          #'ecoin-llama--ring-tick))))

(defun ecoin-llama--warm-up (ring)
  "Send the warm-up with RING's extra, unless the server already has it."
  (unless (equal (cdr (ecoin-llama--ring-extra ring)) ecoin-llama--warmed)
    (when-let* ((target (condition-case nil (ecoin-llama--target)
                          (user-error nil))))
      (ecoin-llama--start
       (ecoin-llama--make-job :warmup t :ring ring :target target)))))

(defun ecoin-llama--note-warmup (job)
  "Record that the warm-up JOB finished."
  (let ((chars (ecoin-llama--extra-chars (ecoin-llama--job-extra job))))
    (ecoin-llama--stat-count 'warmup)
    (setq ecoin-llama--last-warmup
          (list :chunks (length (ecoin-llama--job-extra job)) :chars chars
                :ms (round (* 1000 (- (float-time) (ecoin-llama--job-sent job))))))
    (ecoin--log "warmup: %s" ecoin-llama--last-warmup)))

(defun ecoin-llama--ring-promote ()
  "Move one queued chunk into the ring and warm the server's cache with it.
Only when a background request is allowed and the server is idle; never
probes, since the ring has no business waking a sleeping model."
  (when-let* ((last ecoin-llama--last-ring)
              (ring (car last))
              ((ecoin-llama--ring-queue ring))
              ((ecoin-llama--background-ok-p))
              ((not ecoin-llama--inflight))
              ((not ecoin-llama--queued))
              ((not (ecoin-llama--needs-probe-p))))
    (ecoin-llama--evict-local ring (cdr last) t)
    (when-let* ((chunk (car (ecoin-llama--ring-queue ring))))
      (setf (ecoin-llama--ring-queue ring) (cdr (ecoin-llama--ring-queue ring))
            (ecoin-llama--ring-chunks ring)
            (last (append (ecoin-llama--ring-chunks ring) (list chunk))
                  ecoin-llama-ring-chunks))
      (cl-incf (ecoin-llama--ring-version ring))
      (ecoin-llama--warm-up ring))))

(defun ecoin-llama--ring-tick ()
  "One ring update; stops the timer outside the activity window."
  (if (not (ecoin-llama--ring-alive-p))
      (ecoin-llama--ring-stop)
    (ecoin--hook #'ecoin-llama--poll-kill)
    (ecoin--hook #'ecoin-llama--ring-promote)))

;;;; Backend methods

(defun ecoin-llama--line-suffix-ok-p ()
  "Non-nil if the text after point allows an automatic request."
  (string-match-p ecoin-llama-line-suffix-regexp
                  (buffer-substring-no-properties
                   (point) (line-end-position))))

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
  "Ask the server for completions for REQUEST; CALLBACK gets the items.
An automatic request is answered from the cache when it can be, at once."
  ;; Before the cache lookup, which a model change must invalidate.
  (condition-case nil (ecoin-llama--sync-model) (user-error nil))
  (ecoin-llama--note-activity)
  (let* ((manual (eq (ecoin-request-trigger request) 'manual))
         (ring (ecoin-llama--current-ring t))
         (ctx (and (or manual (ecoin-llama--line-suffix-ok-p))
                   (progn
                     ;; Before the lookup: the cache key includes the extra.
                     (ecoin-llama--evict-local ring (current-buffer))
                     (ecoin-llama--context-for ring)))))
    (when ctx
      (setq ecoin-llama--last-ring (and ring (cons ring (current-buffer))))
      (ecoin-llama--ring-ensure-timer))
    (prog1
        (when ctx
          (if-let* ((hit (and (not manual) (ecoin-llama--cache-lookup ctx))))
              (ecoin-llama--call-with-items callback (car hit))
            ;; Resolving the target first: a changed key or URL ends a backoff.
            (let ((target (ecoin-llama--target-or-nil manual)))
              (when (and target (or manual (not (ecoin-llama--in-backoff-p))))
                (let ((job (ecoin-llama--make-job
                            :buffer (ecoin-request-buffer request)
                            :point (ecoin-request-point request)
                            :tick (ecoin-request-tick request)
                            :callback callback
                            :ctx ctx
                            :ring ring
                            :manual manual
                            :target target)))
                  (ecoin-llama--submit job)
                  job)))))
      (when ctx (ecoin--hook #'ecoin-llama--maybe-far-pick ring)))))

(defconst ecoin-llama--stat-timing-keys
  '(:prompt_n :cache_n :prompt_ms :predicted_n :predicted_ms)
  "Server timings summarized by `ecoin-stats'.")

(cl-defmethod ecoin-backend-stats ((_backend (eql 'llama)))
  "Cache, prefetch and server-timing rows for `ecoin-stats'."
  (let* ((count (lambda (key) (alist-get key ecoin-llama--stat-counts 0)))
         (exact (funcall count 'exact))
         (typed (funcall count 'typed))
         (miss (funcall count 'miss))
         (total (+ exact typed miss))
         (timings (ecoin--ring-items ecoin-llama--stat-timings)))
    (append
     (when (> total 0)
       (list (cons "cache"
                   (format "%d exact, %d typed-through, %d misses = %d%% hit"
                           exact typed miss (round (* 100.0 (+ exact typed)) total)))))
     (list (cons "background"
                 (format "%d prefetches, %d warm-ups"
                         (funcall count 'prefetch) (funcall count 'warmup))))
     (cl-loop for key in ecoin-llama--stat-timing-keys
              for values = (sort (delq nil (mapcar (lambda (tm) (plist-get tm key))
                                                   timings))
                                 #'<)
              when values
              collect (cons (substring (symbol-name key) 1)
                            (format "median %s, p95 %s"
                                    (ecoin-llama--stat-num (ecoin--percentile values 0.5))
                                    (ecoin-llama--stat-num (ecoin--percentile values 0.95))))))))

(defun ecoin-llama--stat-num (n)
  "N, a number, formatted compactly."
  (if (integerp n) (format "%d" n) (format "%.0f" n)))

(cl-defmethod ecoin-backend-stats-reset ((_backend (eql 'llama)))
  (setq ecoin-llama--stat-counts nil)
  (ecoin--ring-clear ecoin-llama--stat-timings))

(cl-defmethod ecoin-backend-shown ((_backend (eql 'llama)) item)
  "Prepare the cache for what the user does with the ghost of ITEM."
  (ecoin-llama--note-shown item))

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

(defun ecoin-llama--ring-status ()
  "Describe the ring of the current buffer's project."
  (if-let* ((ring (and (> ecoin-llama-ring-chunks 0) (ecoin-llama--current-ring))))
      (let ((extra (car (ecoin-llama--ring-extra ring))))
        (format "ring %d chunks / %d queued / %d extra chars"
                (length extra) (length (ecoin-llama--ring-queue ring))
                (ecoin-llama--extra-chars extra)))
    (if (> ecoin-llama-ring-chunks 0)
        "ring 0 chunks / 0 queued / 0 extra chars"
      "ring off")))

(cl-defmethod ecoin-backend-status ((_backend (eql 'llama)))
  "Describe the server and the state of the connection."
  (let* ((server (or (plist-get ecoin-llama--props :model_alias)
                     (plist-get ecoin-llama--props :model_path)))
         (model (if (stringp ecoin-llama-model)
                    (format "%s (server: %s)" ecoin-llama-model
                            (or server "unknown"))
                  (or server "unknown"))))
    (format "backend llama; state %s; url %s; model %s; slots %s; last error %s; backoff %s; %s"
            ecoin-llama--state ecoin-llama-url model
            (or (plist-get ecoin-llama--props :total_slots) "unknown")
            (or ecoin-llama--last-error "none")
            (if (ecoin-llama--in-backoff-p)
                (format "%d s left"
                        (ceiling (- ecoin-llama--backoff-until (float-time))))
              "none")
            (ecoin-llama--ring-status))))

(provide 'ecoin-llama)
;;; ecoin-llama.el ends here
