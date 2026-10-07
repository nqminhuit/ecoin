;;; ecoin-llama-test.el --- Tests for the ecoin llama backend -*- lexical-binding: t; -*-

;;; Commentary:

;; The transport, state machine and single flight are tested against a fake
;; HTTP server in this file (`make-network-process' with `:server t' on
;; 127.0.0.1) that records the raw request bytes, answers from a script and
;; notices when a client closes early.  Context building and post-processing
;; are tested as pure functions on temp buffers.  No llama-server, GPU or
;; network access is needed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ecoin)
(require 'ecoin-llama)

;;;; Fake server

(defvar ecoin-llama-test--server nil "The listening fake server process.")
(defvar ecoin-llama-test--clients nil "Accepted client processes.")
(defvar ecoin-llama-test--requests nil "Requests seen so far, oldest first.")
(defvar ecoin-llama-test--closed 0 "Clients that closed before being answered.")
(defvar ecoin-llama-test--handler nil
  "Function from a request plist to a reply plist.
Reply keys: :status (default 200), :body (plist or string), :delay (seconds),
:hang (never answer), :close (close without answering).")
(defvar ecoin-llama-test--messages nil "Messages shown, newest first.")
(defvar ecoin-llama-test--calls nil "(PATH TIMEOUT) of each `ecoin-llama--http' call.")

(defconst ecoin-llama-test--props '(:total_slots 1 :is_sleeping :false
                                    :model_alias "test-model")
  "A healthy /props answer.")

(defun ecoin-llama-test--parse-request (raw)
  "Return the request plist in RAW bytes, or nil while it is incomplete."
  (when-let* ((sep (string-search "\r\n\r\n" raw)))
    (let* ((head (substring raw 0 sep))
           (body (substring raw (+ sep 4)))
           (case-fold-search t)
           (len (if (string-match "^content-length: *\\([0-9]+\\)" head)
                    (string-to-number (match-string 1 head))
                  0)))
      (when (and (>= (length body) len)
                 (string-match "\\`\\([A-Z]+\\) \\([^ ]+\\) HTTP/1.1" head))
        (list :method (match-string 1 head) :path (match-string 2 head)
              :head head :body body :raw raw)))))

(defun ecoin-llama-test--reply-bytes (reply)
  "The HTTP response bytes for the reply plist REPLY."
  (let* ((status (or (plist-get reply :status) 200))
         (body (plist-get reply :body))
         (payload (cond ((stringp body) (encode-coding-string body 'utf-8))
                        (body (ecoin-llama--encode-body body))
                        (t "")))
         (head (format (concat "HTTP/1.1 %d X\r\nContent-Type: application/json\r\n"
                               "Content-Length: %d\r\nConnection: close\r\n\r\n")
                       status (length payload))))
    (concat (encode-coding-string head 'us-ascii) payload)))

(defun ecoin-llama-test--serve (client request)
  "Answer REQUEST on CLIENT as the handler scripts it."
  (let* ((reply (funcall ecoin-llama-test--handler request))
         (delay (or (plist-get reply :delay) 0))
         (send (lambda ()
                 (when (process-live-p client)
                   (process-put client 'answered t)
                   (process-send-string client
                                        (ecoin-llama-test--reply-bytes reply))
                   (delete-process client)))))
    (cond ((plist-get reply :hang) nil)
          ((plist-get reply :close) (delete-process client))
          ((> delay 0) (run-at-time delay nil send))
          (t (funcall send)))))

(defun ecoin-llama-test--filter (client string)
  "Collect STRING from CLIENT; serve the request once it is complete."
  (process-put client 'raw (concat (process-get client 'raw) string))
  (unless (process-get client 'seen)
    (when-let* ((request (ecoin-llama-test--parse-request
                          (process-get client 'raw))))
      (process-put client 'seen t)
      (setq ecoin-llama-test--requests
            (append ecoin-llama-test--requests (list request)))
      (ecoin-llama-test--serve client request))))

(defun ecoin-llama-test--sentinel (client _event)
  "Count CLIENT as closed early if it went away unanswered."
  (unless (or (process-live-p client) (process-get client 'answered)
              (process-get client 'counted))
    (process-put client 'counted t)
    (cl-incf ecoin-llama-test--closed)))

(defun ecoin-llama-test--start (handler)
  "Start the fake server with HANDLER; return its port."
  (setq ecoin-llama-test--handler handler
        ecoin-llama-test--requests nil
        ecoin-llama-test--clients nil
        ecoin-llama-test--closed 0
        ecoin-llama-test--server
        (make-network-process
         :name "ecoin-test-server" :server t :host "127.0.0.1" :service t
         :family 'ipv4 :coding 'binary :noquery t
         :log (lambda (_server client _message)
                (push client ecoin-llama-test--clients)
                (set-process-coding-system client 'binary 'binary)
                (set-process-query-on-exit-flag client nil)
                (set-process-filter client #'ecoin-llama-test--filter)
                (set-process-sentinel client #'ecoin-llama-test--sentinel))))
  (process-contact ecoin-llama-test--server :service))

(defun ecoin-llama-test--stop ()
  "Stop the fake server and its clients."
  (dolist (c ecoin-llama-test--clients) (ignore-errors (delete-process c)))
  (when ecoin-llama-test--server (delete-process ecoin-llama-test--server))
  (setq ecoin-llama-test--server nil ecoin-llama-test--clients nil))

(defun ecoin-llama-test--default-handler (request)
  "A healthy server: /props is fine, /infill completes with \"ghost\"."
  (if (equal (plist-get request :path) ecoin-llama--path-props)
      (list :body ecoin-llama-test--props)
    (list :body '(:content "ghost"))))

(defun ecoin-llama-test--wait (predicate &optional timeout)
  "Run the event loop until PREDICATE is non-nil or TIMEOUT seconds pass."
  (let ((deadline (+ (float-time) (or timeout 5))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (funcall predicate)))

(defun ecoin-llama-test--settle (&optional seconds)
  "Run the event loop for SECONDS, to show that nothing else happens."
  (ecoin-llama-test--wait #'ignore (or seconds 0.3)))

(defun ecoin-llama-test--http-buffers ()
  "Live buffers of the HTTP client."
  (seq-filter (lambda (b) (string-prefix-p ecoin-llama--http-buffer-name
                                           (buffer-name b)))
              (buffer-list)))

(defun ecoin-llama-test--paths ()
  "Paths of the requests the server has seen, oldest first."
  (mapcar (lambda (r) (plist-get r :path)) ecoin-llama-test--requests))

(defun ecoin-llama-test--count (path)
  "Number of requests to PATH seen so far."
  (seq-count (lambda (p) (equal p path)) (ecoin-llama-test--paths)))

(defun ecoin-llama-test--messages-matching (regexp)
  "Messages shown so far that match REGEXP."
  (seq-filter (lambda (m) (string-match-p regexp m)) ecoin-llama-test--messages))

(defun ecoin-llama-test--prime-ready ()
  "Pretend the server was just seen healthy."
  (setq ecoin-llama--state 'ready
        ecoin-llama--last-ok (float-time)
        ecoin-llama--props ecoin-llama-test--props))

(defmacro ecoin-llama-test--with-server (handler &rest body)
  "Run BODY against a fake server scripted by HANDLER (a function)."
  (declare (indent 1))
  `(let* ((ecoin-llama-test--messages nil)
          (ecoin-llama-test--calls nil)
          (port (ecoin-llama-test--start ,handler))
          (ecoin-llama-url (format "http://127.0.0.1:%d" port))
          (ecoin-llama-api-key nil)
          (ecoin-llama-request-timeout 2)
          (ecoin-llama-retry-delay 0.01)
          (orig-http (symbol-function 'ecoin-llama--http)))
     (ecoin-llama--reset)
     (unwind-protect
         (cl-letf (((symbol-function 'message)
                    (lambda (fmt &rest args)
                      (when fmt (push (apply #'format fmt args)
                                      ecoin-llama-test--messages))))
                   ((symbol-function 'ecoin-llama--http)
                    (lambda (target method path object timeout callback)
                      (push (list path timeout) ecoin-llama-test--calls)
                      (funcall orig-http target method path object timeout
                               callback))))
           ,@body)
       (ecoin-llama--reset)
       (ecoin-llama-test--stop))
     (should-not (ecoin-llama-test--http-buffers))))

(defmacro ecoin-llama-test--in-buffer (content &rest body)
  "Run BODY in a buffer holding CONTENT, point at its end, `ecoin-mode' on."
  (declare (indent 1))
  `(let ((ecoin-backend 'llama))
     (with-temp-buffer
       (insert ,content)
       (ecoin-mode 1)
       (unwind-protect (progn ,@body)
         (ecoin-mode -1)))))

(defun ecoin-llama-test--ghost ()
  "The ghost on display, or nil."
  (and (ecoin--visible-p) (ecoin--overlay-ghost)))

(defun ecoin-llama-test--request-json (request)
  "The JSON body of the fake server's REQUEST as a plist."
  (ecoin-llama--parse-json (decode-coding-string (plist-get request :body)
                                                 'utf-8)))

;;;; Transport

(ert-deftest ecoin-llama-test-body-is-unibyte-utf-8 ()
  (ecoin-llama-test--with-server #'ecoin-llama-test--default-handler
    (ecoin-llama-test--in-buffer "# Việt Nam \U0001F600\nfoo("
      (ecoin--request 'manual)
      (should (ecoin-llama-test--wait #'ecoin-llama-test--ghost))
      (let* ((request (car (last ecoin-llama-test--requests)))
             (body (plist-get request :body))
             (case-fold-search t))
        (should (equal (plist-get request :path) ecoin-llama--path-infill))
        (should-not (multibyte-string-p (plist-get request :raw)))
        (should (decode-coding-string body 'utf-8))
        (should (string-match-p "Việt Nam \U0001F600"
                                (plist-get (ecoin-llama-test--request-json request)
                                           :input_prefix)))
        (string-match "^content-length: *\\([0-9]+\\)" (plist-get request :head))
        (should (= (string-to-number (match-string 1 (plist-get request :head)))
                   (length body)))
        (should (> (length body)
                   (length (decode-coding-string body 'utf-8))))))))

(ert-deftest ecoin-llama-test-request-bytes-are-unibyte ()
  (let* ((ecoin-llama-api-key "abc")
         (target (ecoin-llama--target))
         (request (ecoin-llama--request-bytes
                   target "POST" ecoin-llama--path-infill
                   (ecoin-llama--encode-body '(:prompt "ệ\U0001F600")))))
    (should-not (multibyte-string-p request))
    (should (string-match-p "Authorization: Bearer abc\r\n" request))
    (should (string-match-p "Connection: close\r\n" request))))

(ert-deftest ecoin-llama-test-key-file-with-trailing-newline ()
  (ecoin-llama-test--with-server #'ecoin-llama-test--default-handler
    (let ((file (make-temp-file "ecoin-llama-key")))
      (unwind-protect
          (progn
            (with-temp-file file (insert "secret-key\n"))
            (setq ecoin-llama-api-key file)
            (ecoin-llama-test--in-buffer "foo("
              (ecoin--request 'manual)
              (should (ecoin-llama-test--wait #'ecoin-llama-test--ghost))
              (should (cl-every
                       (lambda (r)
                         (string-match-p "^Authorization: Bearer secret-key\r$"
                                         (plist-get r :head)))
                       ecoin-llama-test--requests))))
        (delete-file file)))))

(ert-deftest ecoin-llama-test-key-function-and-literal ()
  (let ((ecoin-llama-api-key (lambda () "  from-fn  ")))
    (should (equal "from-fn" (ecoin-llama--api-key))))
  (let ((ecoin-llama-api-key "literal-key"))
    (should (equal "literal-key" (ecoin-llama--api-key))))
  (let ((ecoin-llama-api-key "   "))
    (should-not (ecoin-llama--api-key))))

(ert-deftest ecoin-llama-test-key-lookup-ignores-default-directory ()
  (let ((default-directory "/ssh:nohost:/home/")
        (ecoin-llama-api-key "sk-literal")
        (seen nil))
    (cl-letf (((symbol-function 'file-readable-p)
               (lambda (f) (push f seen) nil)))
      (should (equal "sk-literal" (ecoin-llama--api-key))))
    (should seen)
    (should-not (cl-some #'file-remote-p seen))))

(ert-deftest ecoin-llama-test-bad-keys-are-user-errors ()
  (dolist (key '("kéy" "ke\ny" "ke\r\ny" "\U0001F600"))
    (let ((ecoin-llama-api-key key))
      (should-error (ecoin-llama--api-key) :type 'user-error)
      ;; The message must not leak the key.
      (should-not (string-match-p
                   (regexp-quote key)
                   (cadr (should-error (ecoin-llama--api-key)
                                       :type 'user-error)))))))

(ert-deftest ecoin-llama-test-https-is-a-user-error ()
  (let ((ecoin-llama-url "https://127.0.0.1:8012"))
    (should-error (ecoin-llama--target) :type 'user-error))
  (let ((ecoin-llama-url "ftp://nope"))
    (should-error (ecoin-llama--target) :type 'user-error)))

(ert-deftest ecoin-llama-test-url-parsing ()
  (let* ((ecoin-llama-url "http://localhost:9000/base/")
         (target (ecoin-llama--target)))
    (should (equal "localhost" (ecoin-llama--target-host target)))
    (should (= 9000 (ecoin-llama--target-port target)))
    (should (equal "/base" (ecoin-llama--target-base target))))
  (let* ((ecoin-llama-url "http://[::1]")
         (target (ecoin-llama--target)))
    (should (equal "::1" (ecoin-llama--target-host target)))
    (should (= 80 (ecoin-llama--target-port target)))))

(ert-deftest ecoin-llama-test-non-loopback-warns-once ()
  (let ((ecoin-llama-test--messages nil)
        (ecoin-llama--warned-loopback nil)
        (ecoin-llama-url "http://example.com:8012"))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) ecoin-llama-test--messages))))
      (ecoin-llama--target)
      (ecoin-llama--target)
      (should (= 1 (length ecoin-llama-test--messages)))
      (setq ecoin-llama--warned-loopback nil
            ecoin-llama-test--messages nil
            ecoin-llama-warn-non-loopback nil)
      (ecoin-llama--target)
      (should-not ecoin-llama-test--messages)
      (setq ecoin-llama-warn-non-loopback t
            ecoin-llama-url "http://127.0.0.5:1")
      (ecoin-llama--target)
      (should-not ecoin-llama-test--messages))
    (setq ecoin-llama--warned-loopback nil)))

(defun ecoin-llama-test--http-result (target method path object timeout)
  "Run one request to completion and return its result plist."
  (let (result)
    (ecoin-llama--http target method path object timeout
                       (lambda (r) (setq result r)))
    (ecoin-llama-test--wait (lambda () result) (+ timeout 2))
    result))

(ert-deftest ecoin-llama-test-no-buffers-left-after-success-error-timeout ()
  (let ((handler (lambda (request)
                   (pcase (plist-get request :path)
                     ("/ok" '(:body (:a 1)))
                     ("/err" '(:status 500 :body (:error (:message "boom"))))
                     (_ '(:hang t))))))
    (ecoin-llama-test--with-server handler
      (let ((target (ecoin-llama--target)))
        (should (equal '(:status 200 :body (:a 1))
                       (ecoin-llama-test--http-result target "GET" "/ok" nil 2)))
        (should-not (ecoin-llama-test--http-buffers))
        (should (equal '(:status 500 :body (:error (:message "boom")))
                       (ecoin-llama-test--http-result target "POST" "/err"
                                                      '(:x 1) 2)))
        (should-not (ecoin-llama-test--http-buffers))
        (should (equal '(:error timeout)
                       (ecoin-llama-test--http-result target "GET" "/hang"
                                                      nil 0.2)))
        (should-not (ecoin-llama-test--http-buffers))))))

(ert-deftest ecoin-llama-test-refused-and-closed-without-answer ()
  (let ((handler (lambda (_r) '(:close t))))
    (ecoin-llama-test--with-server handler
      (let ((target (ecoin-llama--target)))
        (should (equal '(:error broken)
                       (ecoin-llama-test--http-result target "GET" "/x" nil 2)))
        (should-not (ecoin-llama-test--http-buffers))
        (ecoin-llama-test--stop)
        (should (equal '(:error refused)
                       (ecoin-llama-test--http-result target "GET" "/x" nil 2)))))))

(ert-deftest ecoin-llama-test-cancel-closes-the-socket-without-callback ()
  (ecoin-llama-test--with-server (lambda (_r) '(:hang t))
    (let* ((target (ecoin-llama--target))
           (called nil)
           (conn (ecoin-llama--http target "GET" "/slow" nil 5
                                    (lambda (_r) (setq called t)))))
      (should (ecoin-llama-test--wait (lambda () ecoin-llama-test--requests)))
      (ecoin-llama--http-cancel conn)
      (should-not (ecoin-llama-test--http-buffers))
      (should (ecoin-llama-test--wait (lambda () (= 1 ecoin-llama-test--closed))))
      (ecoin-llama-test--settle 0.2)
      (should-not called))))

(ert-deftest ecoin-llama-test-chunked-response ()
  (let ((handler (lambda (_r) '(:hang t))))
    (ecoin-llama-test--with-server handler
      (let* ((target (ecoin-llama--target))
             (result nil))
        (ecoin-llama--http target "GET" "/chunks" nil 2
                           (lambda (r) (setq result r)))
        (should (ecoin-llama-test--wait (lambda () ecoin-llama-test--requests)))
        (let ((client (car ecoin-llama-test--clients)))
          (process-send-string
           client (concat "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n"
                          "5\r\n{\"a\":\r\n2\r\n1}\r\n0\r\n\r\n")))
        (should (ecoin-llama-test--wait (lambda () result)))
        (should (equal '(:status 200 :body (:a 1)) result))))))

(ert-deftest ecoin-llama-test-json-el-fallback-is-equivalent ()
  (let* ((object (list :s "Việt \U0001F600 \"q\"\n" :n 3 :f 0.9 :t t
                       :no :false :list ["a" "b"] :empty []
                       :nested (list :k 1)))
         (native (ecoin-llama--encode-body object))
         (fallback (let ((ecoin-llama--native-json nil))
                     (ecoin-llama--encode-body object))))
    (should-not (multibyte-string-p native))
    (should-not (multibyte-string-p fallback))
    (should (equal (ecoin-llama--parse-json (decode-coding-string native 'utf-8))
                   (ecoin-llama--parse-json
                    (decode-coding-string fallback 'utf-8))))
    (let* ((json "{\"a\":null,\"b\":false,\"c\":[1,{\"d\":\"é\"}],\"e\":{}}")
           (expected (ecoin-llama--parse-json json)))
      (should (equal expected
                     (let ((ecoin-llama--native-json nil))
                       (ecoin-llama--parse-json json))))
      (should (equal '(1 (:d "é")) (plist-get expected :c))))
    (should-not (ecoin-llama--parse-json "not json"))))

(ert-deftest ecoin-llama-test-json-el-fallback-end-to-end ()
  (ecoin-llama-test--with-server #'ecoin-llama-test--default-handler
    (let ((ecoin-llama--native-json nil))
      (ecoin-llama-test--in-buffer "# Việt \U0001F600\nfoo("
        (ecoin--request 'manual)
        (should (equal "ghost"
                       (progn (ecoin-llama-test--wait #'ecoin-llama-test--ghost)
                              (ecoin-llama-test--ghost))))
        (should (string-match-p
                 "Việt"
                 (plist-get (ecoin-llama-test--request-json
                             (car (last ecoin-llama-test--requests)))
                            :input_prefix)))))))

;;;; Request body

(ert-deftest ecoin-llama-test-request-body-fields ()
  (let* ((ecoin-llama-slot nil)
         (ecoin-llama--props '(:total_slots 1))
         (job (ecoin-llama--make-job
               :ctx (list :prefix "p\n" :middle "m" :suffix "\ns\n"
                          :n-indent 2)))
         (body (ecoin-llama--request-body job))
         (json (ecoin-llama--parse-json (decode-coding-string
                                         (ecoin-llama--encode-body body)
                                         'utf-8))))
    (should (equal "p\n" (plist-get json :input_prefix)))
    (should (equal "\ns\n" (plist-get json :input_suffix)))
    (should (equal "m" (plist-get json :prompt)))
    (should (equal 2 (plist-get json :n_indent)))
    (should (equal 128 (plist-get json :n_predict)))
    (should (equal 250 (plist-get json :t_max_predict_ms)))
    (should (eq t (plist-get json :cache_prompt)))
    (should-not (plist-member json :stream_true))
    (should (null (plist-get json :stream)))
    (should (plist-member json :stream))
    (should (equal '("top_k" "top_p" "infill") (plist-get json :samplers)))
    (should (null (plist-get json :input_extra)))
    (should (plist-member json :input_extra))
    (should (member "timings/prompt_n" (plist-get json :response_fields)))
    (dolist (absent '(:id_slot :n_cmpl :t_max_prompt_ms :n_cache_reuse :stop
                      :model))
      (should-not (plist-member json absent)))
    (should (string-match-p "\"input_extra\":\\[\\]"
                            (ecoin-llama--encode-body body)))))

(ert-deftest ecoin-llama-test-request-body-optional-fields ()
  (let* ((ecoin-llama-slot 0)
         (ecoin-llama-manual-alternatives 3)
         (ecoin-llama--props '(:total_slots 4))
         (ecoin-llama-sampling '(:top_k 10 :stop [] :model "x"
                                 :t_max_prompt_ms 5 :n_cache_reuse 4))
         (ctx (list :prefix "" :middle "" :suffix "\n" :n-indent 0))
         (manual (ecoin-llama--request-body
                  (ecoin-llama--make-job :ctx ctx :manual t)))
         (auto (ecoin-llama--request-body (ecoin-llama--make-job :ctx ctx))))
    (should (= 0 (plist-get manual :id_slot)))
    (should (= 3 (plist-get manual :n_cmpl)))
    (should-not (plist-member auto :n_cmpl))
    (should (= 10 (plist-get manual :top_k)))
    (dolist (absent '(:stop :model :t_max_prompt_ms :n_cache_reuse))
      (should-not (plist-member manual absent)))
    ;; Clamped to the server's slots; one slot means no n_cmpl at all.
    (let ((ecoin-llama--props '(:total_slots 2)))
      (should (= 2 (plist-get (ecoin-llama--request-body
                               (ecoin-llama--make-job :ctx ctx :manual t))
                              :n_cmpl))))
    (let ((ecoin-llama--props '(:total_slots 1)))
      (should-not (plist-member (ecoin-llama--request-body
                                 (ecoin-llama--make-job :ctx ctx :manual t))
                                :n_cmpl)))))

;;;; Server states

(ert-deftest ecoin-llama-test-501-unsupported-then-backoff ()
  (ecoin-llama-test--with-server
      (lambda (r)
        (if (equal (plist-get r :path) ecoin-llama--path-props)
            (list :body ecoin-llama-test--props)
          '(:status 501 :body (:error (:code 501 :type "not_supported_error")))))
    (ecoin-llama-test--in-buffer "foo("
      (ecoin--request 'auto)
      (should (ecoin-llama-test--wait
               (lambda () (eq ecoin-llama--state 'unsupported))))
      (should (= 1 (length (ecoin-llama-test--messages-matching "no FIM tokens"))))
      (should (equal " ecoin[!]" (ecoin--lighter)))
      (let ((seen (length ecoin-llama-test--requests)))
        (insert "x")
        (ecoin--request 'auto)
        (ecoin-llama-test--settle)
        (should (= seen (length ecoin-llama-test--requests))))
      (should (= 1 (length (ecoin-llama-test--messages-matching "no FIM tokens"))))
      (should (ecoin-llama--in-backoff-p))
      (should (string-match-p "state unsupported" (ecoin-backend-status 'llama))))))

(ert-deftest ecoin-llama-test-manual-bypasses-backoff-and-retries ()
  (ecoin-llama-test--with-server
      (lambda (r)
        (if (equal (plist-get r :path) ecoin-llama--path-props)
            (list :body ecoin-llama-test--props)
          '(:status 501 :body (:error (:code 501)))))
    (ecoin-llama-test--in-buffer "foo("
      (ecoin--request 'auto)
      (should (ecoin-llama-test--wait
               (lambda () (eq ecoin-llama--state 'unsupported))))
      (let ((before (ecoin-llama-test--count ecoin-llama--path-infill))
            (first-backoff ecoin-llama--backoff))
        (ecoin--request 'manual)
        (should (ecoin-llama-test--wait
                 (lambda () (> (ecoin-llama-test--count ecoin-llama--path-infill)
                               before))))
        (should (ecoin-llama-test--wait
                 (lambda () (null ecoin-llama--inflight))))
        ;; The repeated failure lengthens the backoff, without a second message.
        (should (> ecoin-llama--backoff first-backoff))
        (should (= 1 (length (ecoin-llama-test--messages-matching
                              "no FIM tokens"))))))))

(ert-deftest ecoin-llama-test-401-unauthorized-clears-on-key-change ()
  (ecoin-llama-test--with-server
      (lambda (r)
        (if (string-match-p "Bearer good" (plist-get r :head))
            (ecoin-llama-test--default-handler r)
          '(:status 401 :body (:error (:code 401)))))
    (setq ecoin-llama-api-key "bad")
    (ecoin-llama-test--in-buffer "foo("
      (ecoin--request 'auto)
      (should (ecoin-llama-test--wait
               (lambda () (eq ecoin-llama--state 'unauthorized))))
      (should (= 1 (length (ecoin-llama-test--messages-matching
                            "rejected the API key"))))
      (let ((seen (length ecoin-llama-test--requests)))
        (insert "x")
        (ecoin--request 'auto)
        (ecoin-llama-test--settle)
        (should (= seen (length ecoin-llama-test--requests))))
      (setq ecoin-llama-api-key "good")
      (insert "y")
      (ecoin--request 'auto)
      (should (ecoin-llama-test--wait #'ecoin-llama-test--ghost))
      (should (eq 'ready ecoin-llama--state)))))

(ert-deftest ecoin-llama-test-503-loading ()
  (ecoin-llama-test--with-server
      (lambda (_r) '(:status 503 :body (:error (:message "Loading model"))))
    (ecoin-llama-test--in-buffer "foo("
      (ecoin--request 'auto)
      (should (ecoin-llama-test--wait
               (lambda () (eq ecoin-llama--state 'loading))))
      (should (= 1 (length (ecoin-llama-test--messages-matching
                            "loading a model"))))
      (should (ecoin-llama--in-backoff-p)))))

(ert-deftest ecoin-llama-test-500-is-an-error-with-the-server-message ()
  (ecoin-llama-test--with-server
      (lambda (r)
        (if (equal (plist-get r :path) ecoin-llama--path-props)
            (list :body ecoin-llama-test--props)
          '(:status 500 :body (:error (:message "bad json")))))
    (ecoin-llama-test--in-buffer "foo("
      (ecoin--request 'auto)
      (should (ecoin-llama-test--wait (lambda () (eq ecoin-llama--state 'error))))
      (should (= 1 (length (ecoin-llama-test--messages-matching
                            "answered 500: bad json")))))))

(ert-deftest ecoin-llama-test-500-context-exceeded-reports-once ()
  (ecoin-llama-test--with-server
      (lambda (r)
        (if (equal (plist-get r :path) ecoin-llama--path-props)
            (list :body ecoin-llama-test--props)
          '(:status 500
            :body (:error (:message "Context size has been exceeded.")))))
    (ecoin-llama-test--in-buffer "foo("
      (dotimes (_ 2)
        (ecoin--request 'manual)
        (should (ecoin-llama-test--wait (lambda () (null ecoin-llama--inflight))))
        (insert "x"))
      (should (eq 'ready ecoin-llama--state))
      (should (= 1 (length (ecoin-llama-test--messages-matching
                            "ran out of context")))))))

(ert-deftest ecoin-llama-test-refused-is-down-with-backoff ()
  (ecoin-llama-test--with-server #'ecoin-llama-test--default-handler
    (ecoin-llama-test--stop)
    (ecoin-llama-test--in-buffer "foo("
      (ecoin--request 'auto)
      (should (ecoin-llama-test--wait (lambda () (eq ecoin-llama--state 'down))))
      (should (= 1 (length (ecoin-llama-test--messages-matching "unreachable"))))
      (should (equal 2 ecoin-llama--backoff))
      (let ((calls (length ecoin-llama-test--calls)))
        (insert "x")
        (ecoin--request 'auto)
        (ecoin-llama-test--settle 0.1)
        (should (= calls (length ecoin-llama-test--calls))))
      ;; After the backoff a trigger probes again; still one message, longer
      ;; backoff.
      (setq ecoin-llama--backoff-until (- (float-time) 1))
      (insert "y")
      (ecoin--request 'auto)
      (should (ecoin-llama-test--wait (lambda () (equal 4 ecoin-llama--backoff))))
      (should (= 1 (length (ecoin-llama-test--messages-matching "unreachable")))))))

(ert-deftest ecoin-llama-test-backoff-doubles-up-to-the-cap ()
  (let ((ecoin-llama--state 'unknown) (ecoin-llama--backoff nil)
        (ecoin-llama--announced nil) (seen nil))
    (cl-letf (((symbol-function 'message) #'ignore))
      (dotimes (_ 8)
        (ecoin-llama--set-state 'down "x")
        (push ecoin-llama--backoff seen)))
    (should (equal '(2 4 8 16 32 60 60 60) (nreverse seen)))
    (ecoin-llama--set-state 'ready)
    (should ecoin-llama--backoff)       ; a probe alone does not reset it
    (should-not ecoin-llama--backoff-until)
    (ecoin-llama--note-success)
    (should-not ecoin-llama--backoff)))

(ert-deftest ecoin-llama-test-sleeping-uses-the-wake-timeout ()
  (ecoin-llama-test--with-server
      (lambda (r)
        (if (equal (plist-get r :path) ecoin-llama--path-props)
            (list :body (plist-put (copy-sequence ecoin-llama-test--props)
                                   :is_sleeping t))
          '(:body (:content "ghost"))))
    (let ((ecoin-llama-wake-timeout 77))
      (ecoin-llama-test--in-buffer "foo("
        (ecoin--request 'auto)
        (should (ecoin-llama-test--wait
                 (lambda () (eq ecoin-llama--state 'sleeping))))
        (should (equal " ecoin[z]" (ecoin--lighter)))
        (should (ecoin-llama-test--wait #'ecoin-llama-test--ghost))
        (should (equal '(77) (mapcar #'cadr (seq-filter
                                             (lambda (c)
                                               (equal (car c) ecoin-llama--path-infill))
                                             ecoin-llama-test--calls))))
        (should (= 1 (length (ecoin-llama-test--messages-matching "waking"))))
        (should (eq 'ready ecoin-llama--state))
        (should (equal " ecoin" (ecoin--lighter)))))))

(ert-deftest ecoin-llama-test-timeout-probes-and-is-not-down ()
  (let ((infills 0))
    (ecoin-llama-test--with-server
        (lambda (r)
          (cond ((equal (plist-get r :path) ecoin-llama--path-props)
                 (list :body ecoin-llama-test--props))
                (t (cl-incf infills) '(:hang t))))
      (let ((ecoin-llama-request-timeout 1))
        (ecoin-llama-test--in-buffer "foo("
          (ecoin--request 'manual)
          (should (ecoin-llama-test--wait (lambda () (null ecoin-llama--inflight))))
          (should (= 1 infills))
          ;; Probe before the request and the probe after the timeout.
          (should (= 2 (ecoin-llama-test--count ecoin-llama--path-props)))
          (should (eq 'ready ecoin-llama--state))
          (should-not (ecoin-llama-test--messages-matching "unreachable")))))))

(ert-deftest ecoin-llama-test-idle-return-wakes-without-unreachable ()
  (ecoin-llama-test--with-server
      (lambda (r)
        (if (equal (plist-get r :path) ecoin-llama--path-props)
            (list :body (plist-put (copy-sequence ecoin-llama-test--props)
                                   :is_sleeping t))
          '(:delay 1.5 :body (:content "ghost"))))
    (let ((ecoin-llama-request-timeout 1)
          (ecoin-llama-wake-timeout 5))
      (ecoin-llama-test--in-buffer "foo("
        ;; Healthy long ago: the server has gone to sleep since.
        (setq ecoin-llama--state 'ready
              ecoin-llama--last-ok (- (float-time)
                                      (* 2 ecoin-llama-reprobe-after)))
        (ecoin--request 'auto)
        (should (ecoin-llama-test--wait #'ecoin-llama-test--ghost))
        (should (equal "ghost" (ecoin-llama-test--ghost)))
        (should-not (ecoin-llama-test--messages-matching "unreachable"))
        (should (= 1 (length (ecoin-llama-test--messages-matching "waking"))))
        (should (= 1 (ecoin-llama-test--count ecoin-llama--path-props)))
        (should (equal '(5) (mapcar #'cadr (seq-filter
                                            (lambda (c)
                                              (equal (car c) ecoin-llama--path-infill))
                                            ecoin-llama-test--calls))))))))

(ert-deftest ecoin-llama-test-recent-contact-skips-the-probe ()
  (ecoin-llama-test--with-server #'ecoin-llama-test--default-handler
    (ecoin-llama-test--in-buffer "foo("
      (ecoin-llama-test--prime-ready)
      (ecoin--request 'auto)
      (should (ecoin-llama-test--wait #'ecoin-llama-test--ghost))
      (should (= 0 (ecoin-llama-test--count ecoin-llama--path-props)))
      (should (= 1 (ecoin-llama-test--count ecoin-llama--path-infill))))))

(ert-deftest ecoin-llama-test-restart-resets-state-and-backoff ()
  (let ((ecoin-llama--warned-loopback t))
    (cl-letf (((symbol-function 'message) #'ignore))
      (ecoin-llama--set-state 'down "x")
      (should ecoin-llama--backoff-until)
      (ecoin-backend-restart 'llama)
      (should (eq 'unknown ecoin-llama--state))
      (should-not ecoin-llama--backoff-until)
      (should-not (ecoin-llama--in-backoff-p))
      (should ecoin-llama--warned-loopback))))

(ert-deftest ecoin-llama-test-config-error-is-reported-once ()
  (let ((ecoin-llama-url "https://127.0.0.1:1")
        (ecoin-llama-test--messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) ecoin-llama-test--messages))))
      (ecoin-llama--reset)
      (ecoin-llama-test--in-buffer "foo("
        (dotimes (_ 3)
          (ecoin--request 'auto))
        (should (= 1 (length ecoin-llama-test--messages)))
        (should (eq 'error ecoin-llama--state))
        (should (equal " ecoin[!]" (ecoin--lighter)))
        (should (string-match-p "ecoin\\[!\\]"
                                (ecoin-llama-test--rendered-lighter)))
        ;; A manual trigger signals, and the core shows the error.
        (ecoin--request 'manual)
        (should (= 2 (length ecoin-llama-test--messages)))
        (setq ecoin-llama-url "http://127.0.0.1:1")
        (ecoin--request 'auto)
        (should-not ecoin-llama--config-failed)
        (ecoin-llama--reset)))))

;; `format-mode-line' returns "" in batch mode, so evaluate the :eval entry the
;; mode registered in `minor-mode-alist' by hand.
(defun ecoin-llama-test--rendered-lighter ()
  "The text of `ecoin-mode's entry in `minor-mode-alist' for this buffer."
  (pcase (cadr (assq 'ecoin-mode minor-mode-alist))
    (`(:eval ,form) (eval form t))))

(ert-deftest ecoin-llama-test-lighter-reaches-the-mode-line ()
  (ecoin-llama-test--in-buffer "foo("
    (setq ecoin-llama--state 'down)
    (should (string-match-p "ecoin\\[!\\]" (ecoin-llama-test--rendered-lighter)))
    (setq ecoin-llama--state 'ready)
    (should-not (string-match-p "ecoin\\[" (ecoin-llama-test--rendered-lighter)))
    (ecoin-llama--reset)))

(ert-deftest ecoin-llama-test-status-and-mode-line ()
  (ecoin-llama--reset)
  (let ((ecoin-llama-url "http://127.0.0.1:1"))
    (setq ecoin-llama--props '(:total_slots 1 :model_alias "m1")
          ecoin-llama--state 'ready)
    (let ((status (ecoin-backend-status 'llama)))
      (should (string-match-p "backend llama" status))
      (should (string-match-p "state ready" status))
      (should (string-match-p "http://127.0.0.1:1" status))
      (should (string-match-p "model m1" status))
      (should (string-match-p "slots 1" status))
      (should (string-match-p "last error none" status))
      (should (string-match-p "backoff none" status))))
  (let ((ecoin-backend 'llama))
    (dolist (case '((ready . " ecoin") (unknown . " ecoin") (loading . " ecoin")
                    (sleeping . " ecoin[z]") (down . " ecoin[!]")
                    (error . " ecoin[!]") (unsupported . " ecoin[!]")
                    (unauthorized . " ecoin[!]")))
      (setq ecoin-llama--state (car case))
      (should (equal (cdr case) (ecoin--lighter)))))
  (ecoin-llama--reset))

(ert-deftest ecoin-llama-test-capabilities ()
  (should (plist-get (ecoin-backend-capabilities 'llama) :trigger-on-move)))

(ert-deftest ecoin-llama-test-the-default-backend-is-llama ()
  (should (eq 'llama (default-value 'ecoin-backend)))
  (should (eq 'ecoin-llama (alist-get 'llama ecoin--backend-features))))

;;;; Gating, single flight, cancellation

(ert-deftest ecoin-llama-test-line-suffix-regexp ()
  (dolist (ok '("" ")" ");" "\"]:" "  )  " "}," "'}" " ;"))
    (should (string-match-p ecoin-llama-line-suffix-regexp ok)))
  (dolist (bad '("foo)" "x" ") x" "foo" "a;"))
    (should-not (string-match-p ecoin-llama-line-suffix-regexp bad))))

(ert-deftest ecoin-llama-test-auto-needs-a-matching-line-suffix ()
  (ecoin-llama-test--with-server #'ecoin-llama-test--default-handler
    (ecoin-llama-test--in-buffer "foo(bar)"
      (ecoin-llama-test--prime-ready)
      (goto-char 5)                     ; before "bar)"
      (ecoin--request 'auto)
      (ecoin-llama-test--settle)
      (should-not ecoin-llama-test--requests)
      ;; Manual ignores the gate.
      (ecoin--request 'manual)
      (should (ecoin-llama-test--wait #'ecoin-llama-test--ghost))
      (should (= 1 (ecoin-llama-test--count ecoin-llama--path-infill))))))

(ert-deftest ecoin-llama-test-three-triggers-in-flight-give-one-follow-up ()
  (let ((n 0))
    (ecoin-llama-test--with-server
        (lambda (_r) (cl-incf n) (list :delay 0.3 :body `(:content ,(format "r%d" n))))
      (ecoin-llama-test--in-buffer "x = "
        (ecoin-llama-test--prime-ready)
        (ecoin--request 'auto)
        (should (ecoin-llama-test--wait (lambda () ecoin-llama-test--requests)))
        (dotimes (_ 3)
          (insert "a")
          (ecoin--request 'auto))
        (should (ecoin-llama-test--wait #'ecoin-llama-test--ghost))
        ;; The first answer was for an older context and is not shown.
        (should (equal "r2" (ecoin-llama-test--ghost)))
        (ecoin-llama-test--settle)
        (should (= 2 (ecoin-llama-test--count ecoin-llama--path-infill)))
        (should-not ecoin-llama--queued)
        (should-not ecoin-llama--inflight)))))

(ert-deftest ecoin-llama-test-trigger-in-another-buffer-cancels-the-request ()
  (let ((n 0))
    (ecoin-llama-test--with-server
        (lambda (_r) (cl-incf n)
          (if (= n 1) '(:hang t) '(:body (:content "second"))))
      (ecoin-llama-test--prime-ready)
      (let ((ecoin-backend 'llama) (a (generate-new-buffer " a"))
            (b (generate-new-buffer " b")))
        (unwind-protect
            (progn
              (with-current-buffer a (insert "aaa(") (ecoin-mode 1)
                                   (ecoin--request 'auto))
              (should (ecoin-llama-test--wait (lambda () ecoin-llama-test--requests)))
              (with-current-buffer b
                (insert "bbb(") (ecoin-mode 1)
                (ecoin--request 'auto)
                (should (ecoin-llama-test--wait #'ecoin-llama-test--ghost))
                (should (equal "second" (ecoin-llama-test--ghost))))
              (should (= 1 ecoin-llama-test--closed)))
          (dolist (buf (list a b))
            (with-current-buffer buf (ecoin-mode -1))
            (kill-buffer buf)))))))

(ert-deftest ecoin-llama-test-far-trigger-cancels-but-near-one-does-not ()
  (let ((n 0) (ecoin-llama-n-suffix 2))
    (ecoin-llama-test--with-server
        (lambda (_r) (cl-incf n)
          (if (<= n 2) '(:hang t) '(:body (:content "third"))))
      (ecoin-llama-test--in-buffer (mapconcat #'identity
                                              (make-list 10 "line(") "\n")
        (ecoin-llama-test--prime-ready)
        (goto-char (point-min)) (end-of-line)
        (ecoin--request 'auto)
        (should (ecoin-llama-test--wait (lambda () ecoin-llama-test--requests)))
        ;; One line away: the request is kept; the new one waits.
        (forward-line 1) (end-of-line)
        (ecoin--request 'auto)
        (ecoin-llama-test--settle 0.1)
        (should (= 0 ecoin-llama-test--closed))
        (should ecoin-llama--queued)
        ;; Far away: the request in flight is closed.
        (goto-char (point-max))
        (ecoin--request 'auto)
        (should (ecoin-llama-test--wait (lambda () (= 1 ecoin-llama-test--closed))))
        (should (ecoin-llama-test--wait (lambda () (>= n 2))))))))

(ert-deftest ecoin-llama-test-disabling-the-mode-cancels-the-request ()
  (ecoin-llama-test--with-server (lambda (_r) '(:hang t))
    (ecoin-llama-test--prime-ready)
    (let ((ecoin-backend 'llama))
      (with-temp-buffer
        (insert "foo(")
        (ecoin-mode 1)
        (ecoin--request 'auto)
        (should (ecoin-llama-test--wait (lambda () ecoin-llama-test--requests)))
        (ecoin-mode -1)
        (should-not ecoin-llama--inflight)
        (should (ecoin-llama-test--wait (lambda () (= 1 ecoin-llama-test--closed))))))))

(ert-deftest ecoin-llama-test-killing-the-buffer-cancels-the-request ()
  (ecoin-llama-test--with-server (lambda (_r) '(:hang t))
    (ecoin-llama-test--prime-ready)
    (let ((ecoin-backend 'llama) (buf (generate-new-buffer " k")))
      (with-current-buffer buf
        (insert "foo(")
        (ecoin-mode 1)
        (ecoin--request 'auto))
      (should (ecoin-llama-test--wait (lambda () ecoin-llama-test--requests)))
      (kill-buffer buf)
      (should-not ecoin-llama--inflight)
      (should (ecoin-llama-test--wait (lambda () (= 1 ecoin-llama-test--closed)))))))

(ert-deftest ecoin-llama-test-a-reply-for-a-moved-point-is-dropped ()
  (ecoin-llama-test--with-server
      (lambda (_r) '(:delay 0.2 :body (:content "late")))
    (ecoin-llama-test--prime-ready)
    (ecoin-llama-test--in-buffer "foo("
      (ecoin--request 'auto)
      (should (ecoin-llama-test--wait (lambda () ecoin-llama-test--requests)))
      (insert "x")
      (ecoin-llama-test--wait (lambda () (null ecoin-llama--inflight)))
      (should-not (ecoin-llama-test--ghost)))))

(ert-deftest ecoin-llama-test-whitespace-line-is-not-dedented-twice ()
  (ecoin-llama-test--with-server
      (lambda (r)
        (if (equal (plist-get r :path) ecoin-llama--path-props)
            (list :body ecoin-llama-test--props)
          '(:body (:content "        return 1\n"))))
    (ecoin-llama-test--in-buffer "def f():\n    "
      (ecoin--request 'auto)
      (should (ecoin-llama-test--wait #'ecoin-llama-test--ghost))
      (should (equal "    return 1" (ecoin-llama-test--ghost)))
      (ecoin-accept)
      (should (equal "def f():\n        return 1" (buffer-string))))))

(ert-deftest ecoin-llama-test-manual-alternatives-are-cycled ()
  (ecoin-llama-test--with-server
      (lambda (r)
        (if (equal (plist-get r :path) ecoin-llama--path-props)
            (list :body (plist-put (copy-sequence ecoin-llama-test--props)
                                   :total_slots 4))
          '(:body [(:content "one") (:content "two") (:content "one ")
                   (:content "")])))
    (ecoin-llama-test--in-buffer "x = "
      (ecoin--request 'manual)
      (should (ecoin-llama-test--wait #'ecoin-llama-test--ghost))
      (should (= 3 (plist-get (ecoin-llama-test--request-json
                               (car (last ecoin-llama-test--requests)))
                              :n_cmpl)))
      (should (equal "one" (ecoin-llama-test--ghost)))
      (ecoin-next)
      (should (equal "two" (ecoin-llama-test--ghost)))
      (ecoin-next)
      (should (equal "one" (ecoin-llama-test--ghost))))))

;;;; Context

(defmacro ecoin-llama-test--context (content &rest body)
  "Run BODY in a buffer holding CONTENT; a \"|\" in it marks point."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     (search-forward "|")
     (delete-char -1)
     (let ((ctx (ecoin-llama--context)))
       (ignore ctx)
       ,@body)))

(ert-deftest ecoin-llama-test-context-line-windows ()
  (let ((ecoin-llama-n-prefix 2) (ecoin-llama-n-suffix 2))
    (ecoin-llama-test--context "l1\nl2\nl3\nl4\nl|5\nl6\nl7\nl8\n"
      (should (equal "l3\nl4\n" (plist-get ctx :prefix)))
      (should (equal "l" (plist-get ctx :middle)))
      (should (equal "5\nl6\nl7\n" (plist-get ctx :suffix))))))

(ert-deftest ecoin-llama-test-context-indent-and-middle ()
  (ecoin-llama-test--context "x\n    foo(|\ny"
    (should (equal "    foo(" (plist-get ctx :middle)))
    (should (= 4 (plist-get ctx :n-indent)))
    (should (equal "x\n" (plist-get ctx :prefix)))
    (should (equal "\ny\n" (plist-get ctx :suffix))))
  (ecoin-llama-test--context "\t\tfoo|"
    (should (= 2 (plist-get ctx :n-indent)))))

(ert-deftest ecoin-llama-test-context-whitespace-only-line ()
  (ecoin-llama-test--context "x\n    |\ny\n"
    (should (equal "" (plist-get ctx :middle)))
    (should (= 0 (plist-get ctx :n-indent)))
    (should (equal "\ny\n" (plist-get ctx :suffix))))
  (ecoin-llama-test--context "x\n  | \ny\n"
    (should (equal "" (plist-get ctx :middle)))
    (should (equal " \ny\n" (plist-get ctx :suffix)))))

(ert-deftest ecoin-llama-test-context-uses-the-widened-buffer ()
  (with-temp-buffer
    (insert "one\ntwo\nthree\nfour\nfive")
    (goto-char (point-min))
    (forward-line 2)
    (end-of-line)
    (narrow-to-region (line-beginning-position) (point))
    (let ((ctx (ecoin-llama--context)))
      (should (equal "one\ntwo\n" (plist-get ctx :prefix)))
      (should (equal "three" (plist-get ctx :middle)))
      (should (equal "\nfour\nfive\n" (plist-get ctx :suffix))))
    (should (buffer-narrowed-p))))

(ert-deftest ecoin-llama-test-context-buffer-edges ()
  (ecoin-llama-test--context "|abc"
    (should (equal "" (plist-get ctx :prefix)))
    (should (equal "" (plist-get ctx :middle)))
    (should (equal "abc\n" (plist-get ctx :suffix))))
  (ecoin-llama-test--context "abc\n|"
    (should (equal "abc\n" (plist-get ctx :prefix)))
    (should (equal "\n" (plist-get ctx :suffix))))
  (ecoin-llama-test--context "abc|"
    (should (stringp (plist-get ctx :suffix)))
    (should (equal "\n" (plist-get ctx :suffix))))
  (with-temp-buffer
    (should (equal "\n" (plist-get (ecoin-llama--context) :suffix)))))

(ert-deftest ecoin-llama-test-context-char-caps-cut-at-line-boundaries ()
  (let ((ecoin-llama-max-prefix-chars 5))
    (ecoin-llama-test--context "aaaa\nbbbb\ncccc\nx|"
      (should (equal "cccc\n" (plist-get ctx :prefix)))))
  (let ((ecoin-llama-max-prefix-chars 7))
    (ecoin-llama-test--context "aaaa\nbbbb\ncccc\nx|"
      (should (equal "cccc\n" (plist-get ctx :prefix)))))
  (let ((ecoin-llama-max-prefix-chars 3))
    (ecoin-llama-test--context "aaaa\nbbbb\ncccc\nx|"
      (should (equal "" (plist-get ctx :prefix)))))
  (let ((ecoin-llama-max-suffix-chars 11))
    (ecoin-llama-test--context "x|\naaaa\nbbbb\ncccc\n"
      (should (equal "\naaaa\nbbbb\n" (plist-get ctx :suffix)))))
  (let ((ecoin-llama-max-suffix-chars 2))
    (ecoin-llama-test--context "x|\naaaa\nbbbb\n"
      (should (equal "\n" (plist-get ctx :suffix))))))

(ert-deftest ecoin-llama-test-context-is-fast-in-a-big-buffer ()
  (with-temp-buffer
    (dotimes (_ 20000) (insert "some line of code here\n"))
    (let ((start (float-time)))
      (dotimes (_ 20) (ecoin-llama--context))
      (should (< (- (float-time) start) 1.0)))))

;;;; Post-processing

(defun ecoin-llama-test--post (buffer content &optional keep-indent)
  "Post-process CONTENT in a temp BUFFER whose \"|\" marks point."
  (with-temp-buffer
    (insert buffer)
    (goto-char (point-min))
    (search-forward "|")
    (delete-char -1)
    (ecoin-llama--postprocess content keep-indent)))

(defconst ecoin-llama-test--post-cases
  '(;; Rule 1: control tokens that leaked into the content.
    ("x = |" "1<|endoftext|>" "1")
    ("x = |" "a<|file_sep|>b<|fim_middle|>c<|im_end|>" "abc")
    ("x = |" "foo<|cursor|>bar" "foobar")
    ("x = |" "<|endoftext|>" nil)
    ;; Rule 2: trailing blank lines and whitespace, empty result.
    ("x = |" "1  \n\n  \n" "1")
    ("x = |" "a\n  b   \n\n" "a\n  b")
    ("x = |" "\n\n" nil)
    ("x = |" "" nil)
    ("f(|" "\n    bar\n" "\n    bar")
    ;; Rule 3: indentation already before point on a whitespace-only line.
    ("def f():\n    |" "        return 1" "    return 1")
    ("    |" "  x" "x")
    ("\t|" "\t\ty" "\ty")
    ("def f():\n    |" "return 1" "return 1")
    ;; Rule 4a: a single line that is the text after point.
    ("f(|)" ")" nil)
    ("f(|)" " )" nil)
    ("f(|)" "a)" "a")
    ;; Rule 4b: empty first line and the following lines are in the buffer.
    ("a|\nb\nc" "\nb\nc" nil)
    ("a|\nb\nc" "\nb\nd" "\nb\nd")
    ;; Rule 4c: the suggestion rewrites the next non-blank line.
    ("    |\n\n    foo" "foo" nil)
    ("    |\n    foo\nbarbaz" "foo\nbar" nil)
    ("    |\n    foo\nbar\nbaz" "foo\nbar\nbaz" nil)
    ("    |\n    foo\nbar\nbaz" "foo\nbar\nqux" "foo\nbar\nqux")
    ("    |\n    foo" "fox" "fox")
    ;; Rule 5: repetition.
    ("|" "a\nb\nb\nc" "a\nb")
    ("|" "a\n\n\nb" "a\n\n\nb")
    ("|" "x\nabcabcabcabcabcabcabc" nil)
    ("|" "0123456789012345678901234567890123456789" nil)
    ("|" "abcabcabc" "abcabcabc")
    ;; Rule 6: text after point keeps only the first line, minus the closers.
    ("f(|)" "a, b)\nnext" "a, b")
    ("f(|)" "a, b" "a, b")
    ("f(|) # c" "1\n2" "1")
    ("f(|)" "\nfoo" nil)
    ("f(|);" "x);" "x")
    ;; Rule 7: closers the buffer already has.
    ("if (x) {\n    |\n}\n" "foo();\n}" "foo();")
    ("if (x) {\n    |\n}\n" "foo();\n  }" "foo();\n  }")
    ("x {\n|\n\n}\n" "a\n\n}" "a")
    ("f(|\n)" "a,\n b\n)" "a,\n b")
    ("do\n  |\nend\n" "work\nend" "work")
    ("do\n  |\nend\nend\n" "work\nend" "work")
    ("x\n  |\n  }\n}\n" "a\n  }\n}" "a")
    ("do\n  |\nfoo\n" "work\nend" "work\nend")
    ;; Nothing special: multi-line text passes through.
    ("def f():\n    |" "a = 1\n    b = 2" "a = 1\n    b = 2"))
  "(BUFFER CONTENT EXPECTED) for the post-processing rules.")

(ert-deftest ecoin-llama-test-postprocess-rules ()
  (dolist (case ecoin-llama-test--post-cases)
    (pcase-let ((`(,buffer ,content ,expected) case))
      (should (equal expected (ecoin-llama-test--post buffer content))))))

(ert-deftest ecoin-llama-test-postprocess-keep-indent ()
  (should (equal "    " (substring (ecoin-llama-test--post "    |" "        x" t)
                                   0 4)))
  (should (equal "        x" (ecoin-llama-test--post "    |" "        x" t))))

(ert-deftest ecoin-llama-test-items-drop-empties-and-duplicates ()
  (with-temp-buffer
    (insert "x = ")
    (let ((items (ecoin-llama--items '("a" "a " "b" "" "<|endoftext|>" "a"))))
      (should (equal '("a" "b") (mapcar #'ecoin-item-text items)))
      (should (cl-every (lambda (i) (null (ecoin-item-end i))) items))
      (should (cl-every (lambda (i) (eq 'llama (ecoin-item-backend i))) items)))))

(ert-deftest ecoin-llama-test-contents-of-a-response ()
  (should (equal '("a") (ecoin-llama--contents '(:content "a" :stop_type "eos"))))
  (should (equal '("a" "b") (ecoin-llama--contents '((:content "a") (:content "b")))))
  (should-not (ecoin-llama--contents nil))
  (should-not (ecoin-llama--contents '(:content nil))))

(provide 'ecoin-llama-test)
;;; ecoin-llama-test.el ends here
