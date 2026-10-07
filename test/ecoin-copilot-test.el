;;; ecoin-copilot-test.el --- Tests for the ecoin Copilot backend -*- lexical-binding: t; -*-

;;; Commentary:

;; Covers the Copilot backend's pure helpers only: position/offset
;; conversion, the language-id and indent-width guesses, status handling,
;; connect backoff, the stale-cache purge and the conversion of LSP items to
;; `ecoin-item'.  Nothing here starts `copilot-language-server' or touches the
;; network -- every path that would (`ecoin-copilot--notify', and therefore
;; `ecoin-copilot--conn') is stubbed out, since a real connection needs the CLI
;; installed and a signed-in account, neither of which CI has.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ecoin-copilot)

;; `python-indent-offset' is not bound in a plain `-Q --batch' run (python.el
;; is never loaded), so `let'-binding it without this would create a lexical
;; binding under `lexical-binding: t' -- invisible to `ecoin-copilot--indent-width's
;; `boundp'/`symbol-value' lookup on the real dynamic variable of that name.
(defvar python-indent-offset)

;;;; ecoin-copilot--utf16-length

(ert-deftest ecoin-copilot-test-utf16-length-ascii ()
  (should (= 5 (ecoin-copilot--utf16-length "hello"))))

(ert-deftest ecoin-copilot-test-utf16-length-counts-astral-chars-twice ()
  ;; U+1F600 is outside the BMP, so UTF-16 needs a surrogate pair for it.
  (should (= 2 (ecoin-copilot--utf16-length "\U0001F600"))))

(ert-deftest ecoin-copilot-test-utf16-length-mixed ()
  (should (= 4 (ecoin-copilot--utf16-length "a\U0001F600b"))))

;;;; ecoin-copilot--lsp-position / ecoin-copilot--from-lsp-position

(ert-deftest ecoin-copilot-test-lsp-position-line-and-character ()
  (with-temp-buffer
    (insert "line one\nline two\nline three")
    (goto-char (point-min))
    (forward-line 1)
    (forward-char 5)
    (should (equal (list :line 1 :character 5) (ecoin-copilot--lsp-position)))))

(ert-deftest ecoin-copilot-test-lsp-position-round-trips-through-astral-char ()
  (with-temp-buffer
    (insert "a\U0001F600bc\nsecond line")
    (dolist (pos (list (point-min)
                        (+ (point-min) 1)   ; between "a" and the emoji
                        (+ (point-min) 2)   ; between the emoji and "b"
                        (+ (point-min) 3)   ; between "b" and "c"
                        (+ (point-min) 4)   ; end of the first line
                        (point-max)))
      (should (= pos (ecoin-copilot--from-lsp-position (ecoin-copilot--lsp-position pos)))))))

;;;; ecoin-copilot--language-id / ecoin-major-mode-alist

(ert-deftest ecoin-copilot-test-language-id-known-mapping ()
  (with-temp-buffer
    (setq major-mode 'emacs-lisp-mode)
    (should (equal "elisp" (ecoin-copilot--language-id)))))

(ert-deftest ecoin-copilot-test-language-id-strips-ts-suffix-then-maps ()
  (with-temp-buffer
    (setq major-mode 'tsx-ts-mode)
    (should (equal "typescriptreact" (ecoin-copilot--language-id)))))

(ert-deftest ecoin-copilot-test-language-id-maps-symbol-heavy-name ()
  (with-temp-buffer
    (setq major-mode 'c++-mode)
    (should (equal "cpp" (ecoin-copilot--language-id)))))

(ert-deftest ecoin-copilot-test-language-id-falls-back-to-mode-name ()
  ;; Not every mode needs a mapping: "python" is already a valid LSP
  ;; languageId, so `ecoin-major-mode-alist' has no entry for it.
  (with-temp-buffer
    (setq major-mode 'python-mode)
    (should (equal "python" (ecoin-copilot--language-id)))))

;;;; ecoin-copilot--indent-width

(ert-deftest ecoin-copilot-test-indent-width-prefers-language-var-over-standard-indent ()
  (let ((python-indent-offset 7)
        (standard-indent 4))
    (with-temp-buffer
      (should (= 7 (ecoin-copilot--indent-width))))))

(ert-deftest ecoin-copilot-test-indent-width-falls-back-to-standard-indent ()
  (let ((python-indent-offset nil)
        (standard-indent 9))
    (with-temp-buffer
      (should (= 9 (ecoin-copilot--indent-width))))))

(ert-deftest ecoin-copilot-test-indent-width-falls-back-to-tab-width ()
  ;; Forcing `standard-indent' non-integer simulates every var in the
  ;; priority list being absent, so the `or' must reach `tab-width'.
  (let ((python-indent-offset nil)
        (standard-indent nil)
        (tab-width 5))
    (with-temp-buffer
      (should (= 5 (ecoin-copilot--indent-width))))))

;;;; Status notifications
;;
;; Payloads below are copied verbatim from a real copilot-language-server
;; 1.544.0, signed in and signed out.  It sends BOTH shapes for one condition,
;; which is why the handler has to suppress the duplicate.

(defmacro ecoin-copilot-test--with-status (&rest body)
  "Run BODY with the status variables reset, returning the messages it emitted."
  (declare (indent 0))
  `(let ((ecoin-copilot--status nil)
         (ecoin-copilot--status-message nil)
         (ecoin-copilot--status-v2 nil)
         (captured '()))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args) (push (apply #'format fmt args) captured))))
       ,@body)
     (nreverse captured)))

(defconst ecoin-copilot-test--v2-normal
  '(:statuses [(:category "cls" :kind "Normal" :inactive :json-false)
               (:category "completion" :busy :json-false)]))

(defconst ecoin-copilot-test--v2-auth-error
  '(:statuses [(:category "auth" :kind "Error"
                :message "You are not signed into GitHub."
                :askToReSignin :json-false
                :result (:status "NotSignedIn"))]))

(defconst ecoin-copilot-test--flat-error
  '(:busy :json-false :kind "Error"
    :message "You are not signed into GitHub."))

(ert-deftest ecoin-copilot-test-status-v2-error-is-reported-with-a-login-hint ()
  (let ((msgs (ecoin-copilot-test--with-status
                (ecoin-copilot--handle-notification nil 'didChangeStatus/v2
                                            ecoin-copilot-test--v2-auth-error))))
    (should (= 1 (length msgs)))
    (should (string-match-p "not signed into GitHub" (car msgs)))
    (should (string-match-p "ecoin-login" (car msgs)))))

(ert-deftest ecoin-copilot-test-status-is-reported-once-across-both-shapes ()
  "The server sends v2 and the flat form for one condition; report once."
  (let ((msgs (ecoin-copilot-test--with-status
                (ecoin-copilot--handle-notification nil 'didChangeStatus/v2
                                            ecoin-copilot-test--v2-normal)
                (ecoin-copilot--handle-notification nil 'didChangeStatus
                                            ecoin-copilot-test--flat-error)
                (ecoin-copilot--handle-notification nil 'didChangeStatus/v2
                                            ecoin-copilot-test--v2-auth-error))))
    (should (= 1 (length msgs)))
    (should (string-match-p "ecoin-login" (car msgs)))))

(ert-deftest ecoin-copilot-test-status-flat-form-still-works-without-v2 ()
  "A server that never sends v2 must still surface its errors."
  (let ((msgs (ecoin-copilot-test--with-status
                (ecoin-copilot--handle-notification nil 'didChangeStatus
                                            ecoin-copilot-test--flat-error))))
    (should (= 1 (length msgs)))
    (should (string-match-p "not signed into GitHub" (car msgs)))))

(ert-deftest ecoin-copilot-test-status-normal-is-silent-and-clears-the-error ()
  (let ((msgs (ecoin-copilot-test--with-status
                (ecoin-copilot--handle-notification nil 'didChangeStatus
                                            ecoin-copilot-test--flat-error)
                (ecoin-copilot--handle-notification nil 'didChangeStatus
                                            '(:kind "Normal" :busy :json-false))
                ;; the same error again, now that it has cleared, reports again
                (ecoin-copilot--handle-notification nil 'didChangeStatus
                                            ecoin-copilot-test--flat-error))))
    (should (= 2 (length msgs)))))

(ert-deftest ecoin-copilot-test-status-v2-records-the-auth-result ()
  (ecoin-copilot-test--with-status
    (ecoin-copilot--handle-notification nil 'didChangeStatus/v2 ecoin-copilot-test--v2-auth-error)
    (should (equal "NotSignedIn"
                   (plist-get (plist-get ecoin-copilot--status :result) :status)))))

(ert-deftest ecoin-copilot-test-status-v2-without-an-auth-entry-is-ignored ()
  (let ((msgs (ecoin-copilot-test--with-status
                (ecoin-copilot--handle-notification nil 'didChangeStatus/v2
                                            ecoin-copilot-test--v2-normal))))
    (should (null msgs))))

;;;; ecoin-copilot--conn readiness and connect backoff

(ert-deftest ecoin-copilot-test-conn-withholds-connection-until-ready ()
  "`ecoin-copilot--conn' reports nothing until `initialize' has been answered.
Callers on the typing path must stay quiet rather than send `didOpen'
into a server that has not finished its handshake."
  (let ((ecoin-copilot--connection 'fake)
        (ecoin-copilot--ready nil)
        (ecoin-copilot--last-connect-attempt nil))
    (cl-letf (((symbol-function 'jsonrpc-running-p) (lambda (_) t))
              ((symbol-function 'ecoin-copilot--connect)
               (lambda () (error "must not reconnect while the process is live"))))
      (should-not (ecoin-copilot--conn))
      (setq ecoin-copilot--ready t)
      (should (eq 'fake (ecoin-copilot--conn))))))

(ert-deftest ecoin-copilot-test-conn-backs-off-after-a-failed-connect ()
  "A server that never comes up is not respawned on every idle tick.
Without the backoff this spawned one node process per keystroke, since
`ecoin-copilot--conn' no longer blocks on the handshake."
  (let ((ecoin-copilot--connection nil)
        (ecoin-copilot--ready nil)
        (ecoin-copilot--last-connect-attempt nil)
        (calls 0))
    (cl-letf (((symbol-function 'ecoin-copilot--connect)
               (lambda () (setq calls (1+ calls)) nil)))
      (dotimes (_ 5) (ecoin-copilot--conn))
      (should (= 1 calls))
      ;; ...but it does try again once the backoff has elapsed.
      (setq ecoin-copilot--last-connect-attempt
            (- (float-time) (1+ ecoin-copilot--connect-backoff)))
      (ecoin-copilot--conn)
      (should (= 2 calls)))))

;;;; ecoin-copilot--purge-stale-cache

;; The guard matters more than the deletion: this runs against a path in the
;; user's config directory, so it must never remove anything real.

(defmacro ecoin-copilot-test--with-fake-cache (&rest body)
  "Run BODY with `ecoin-purge-path-before-connect' inside a temp directory."
  (declare (indent 0))
  `(let* ((tmp (make-temp-file "ecoin-purge" t))
          (ecoin-purge-path-before-connect (expand-file-name "github" tmp)))
     (unwind-protect (progn ,@body)
       (delete-directory tmp t))))

(ert-deftest ecoin-copilot-test-purge-removes-empty-directory ()
  (ecoin-copilot-test--with-fake-cache
    (make-directory ecoin-purge-path-before-connect t)
    (ecoin-copilot--purge-stale-cache)
    (should-not (file-exists-p ecoin-purge-path-before-connect))))

(ert-deftest ecoin-copilot-test-purge-removes-nested-but-fileless-directories ()
  ;; What the server actually leaves behind: owner/repo/agents, all empty.
  (ecoin-copilot-test--with-fake-cache
    (make-directory (expand-file-name "owner/repo/agents"
                                      ecoin-purge-path-before-connect)
                    t)
    (ecoin-copilot--purge-stale-cache)
    (should-not (file-exists-p ecoin-purge-path-before-connect))))

(ert-deftest ecoin-copilot-test-purge-removes-a-plain-file ()
  ;; A file at that path wedges the server just as a directory does.
  (ecoin-copilot-test--with-fake-cache
    (with-temp-file ecoin-purge-path-before-connect (insert ""))
    (ecoin-copilot--purge-stale-cache)
    (should-not (file-exists-p ecoin-purge-path-before-connect))))

(ert-deftest ecoin-copilot-test-purge-keeps-a-directory-holding-real-data ()
  (ecoin-copilot-test--with-fake-cache
    (let ((deep (expand-file-name "owner/repo" ecoin-purge-path-before-connect)))
      (make-directory deep t)
      (with-temp-file (expand-file-name "index.json" deep) (insert "{}"))
      (ecoin-copilot--purge-stale-cache)
      (should (file-exists-p ecoin-purge-path-before-connect))
      (should (file-exists-p (expand-file-name "index.json" deep))))))

(ert-deftest ecoin-copilot-test-purge-unlinks-a-symlink-without-touching-its-target ()
  (ecoin-copilot-test--with-fake-cache
    (let ((target (expand-file-name
                   "real-data"
                   (file-name-directory ecoin-purge-path-before-connect))))
      (make-directory target t)
      (with-temp-file (expand-file-name "keep.json" target) (insert "{}"))
      (make-symbolic-link target ecoin-purge-path-before-connect)
      (ecoin-copilot--purge-stale-cache)
      (should-not (file-exists-p ecoin-purge-path-before-connect))
      (should (file-exists-p (expand-file-name "keep.json" target))))))

(ert-deftest ecoin-copilot-test-purge-is-a-noop-when-the-path-is-absent ()
  (ecoin-copilot-test--with-fake-cache
    (ecoin-copilot--purge-stale-cache)
    (should-not (file-exists-p ecoin-purge-path-before-connect))))

(ert-deftest ecoin-copilot-test-purge-respects-the-nil-opt-out ()
  (ecoin-copilot-test--with-fake-cache
    (make-directory ecoin-purge-path-before-connect t)
    (let ((kept ecoin-purge-path-before-connect)
          (ecoin-purge-path-before-connect nil))
      (ecoin-copilot--purge-stale-cache)
      (should (file-exists-p kept)))))

;;;; Item conversion and accept telemetry

(defun ecoin-copilot-test--result (text start-char end-char)
  "An inline-completion result with TEXT replacing START-CHAR..END-CHAR on line 0."
  `(:items [(:insertText ,text
             :range (:start (:line 0 :character ,start-char)
                     :end (:line 0 :character ,end-char)))]))

(ert-deftest ecoin-copilot-test-items-strip-the-typed-prefix ()
  (with-temp-buffer
    (insert "foo(ba")
    (let* ((items (ecoin-copilot--items
                   (ecoin-copilot-test--result "foo(bar)" 0 6)))
           (item (car items)))
      (should (= 1 (length items)))
      (should (equal "r)" (ecoin-item-text item)))
      (should (= (point) (ecoin-item-end item)))
      (should (eq 'copilot (ecoin-item-backend item))))))

(ert-deftest ecoin-copilot-test-items-end-covers-the-replaced-range ()
  (with-temp-buffer
    (insert "ab XYZ")
    (goto-char 3)
    (let ((item (car (ecoin-copilot--items
                      (ecoin-copilot-test--result "ab foo" 0 6)))))
      (should (equal " foo" (ecoin-item-text item)))
      (should (= 7 (ecoin-item-end item))))))

(ert-deftest ecoin-copilot-test-items-drop-those-not-extending-the-buffer ()
  (with-temp-buffer
    (insert "hello")
    (should-not (ecoin-copilot--items (ecoin-copilot-test--result "hello" 0 5)))
    (should-not (ecoin-copilot--items (ecoin-copilot-test--result "other" 0 5)))
    (should-not (ecoin-copilot--items '(:items [])))))

(ert-deftest ecoin-copilot-test-partial-accept-reports-utf16-length-from-range-start ()
  (with-temp-buffer
    (insert "a\U0001F600b")
    (let* ((item (car (ecoin-copilot--items
                       (ecoin-copilot-test--result "a\U0001F600bcd" 0 4))))
           sent)
      (insert "c")
      (cl-letf (((symbol-function 'ecoin-copilot--notify)
                 (lambda (method params) (setq sent (cons method params)))))
        (ecoin-backend-accepted 'copilot item "c" t))
      (should (eq :textDocument/didPartiallyAcceptCompletion (car sent)))
      ;; "a", the emoji (2 units), "b" and "c".
      (should (= 5 (plist-get (cdr sent) :acceptedLength))))))

(ert-deftest ecoin-copilot-test-full-accept-runs-the-item-command ()
  (let ((item (ecoin-make-item
               :text "x" :backend 'copilot
               :data '(:raw (:command (:command "telemetry" :arguments [1]))
                       :start 1)))
        sent)
    (cl-letf (((symbol-function 'ecoin-copilot--conn) (lambda () 'conn))
              ((symbol-function 'jsonrpc-async-request)
               (lambda (_conn method params &rest _) (setq sent (list method params)))))
      (ecoin-backend-accepted 'copilot item "x" nil))
    (should (eq :workspace/executeCommand (car sent)))
    (should (equal "telemetry" (plist-get (cadr sent) :command)))))

(provide 'ecoin-copilot-test)
;;; ecoin-copilot-test.el ends here
