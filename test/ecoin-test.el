;;; ecoin-test.el --- Tests for ecoin -*- lexical-binding: t; -*-

;;; Commentary:

;; Covers ecoin's pure helpers only: position/offset conversion, the
;; language-id and indent-width guesses, and the accept-word/accept-line
;; transforms.  Nothing here starts `copilot-language-server' or touches the
;; network -- every path that would (`ecoin--notify', and therefore
;; `ecoin--conn') is stubbed out, since a real connection needs the CLI
;; installed and a signed-in account, neither of which CI has.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ecoin)

;; `python-indent-offset' is not bound in a plain `-Q --batch' run (python.el
;; is never loaded), so `let'-binding it without this would create a lexical
;; binding under `lexical-binding: t' -- invisible to `ecoin--indent-width's
;; `boundp'/`symbol-value' lookup on the real dynamic variable of that name.
(defvar python-indent-offset)

(defun ecoin-test--show-ghost (ghost)
  "Fake a visible completion overlay showing GHOST at point, for accept tests."
  (setq ecoin--overlay (make-overlay (point) (point)))
  (overlay-put ecoin--overlay 'ecoin-ghost ghost)
  (overlay-put ecoin--overlay 'ecoin-item nil)
  (overlay-put ecoin--overlay 'ecoin-prefix-len 0)
  (overlay-put ecoin--overlay 'ecoin-end (copy-marker (point))))

;;;; ecoin--utf16-length

(ert-deftest ecoin-test-utf16-length-ascii ()
  (should (= 5 (ecoin--utf16-length "hello"))))

(ert-deftest ecoin-test-utf16-length-counts-astral-chars-twice ()
  ;; U+1F600 is outside the BMP, so UTF-16 needs a surrogate pair for it.
  (should (= 2 (ecoin--utf16-length "\U0001F600"))))

(ert-deftest ecoin-test-utf16-length-mixed ()
  (should (= 4 (ecoin--utf16-length "a\U0001F600b"))))

;;;; ecoin--lsp-position / ecoin--from-lsp-position

(ert-deftest ecoin-test-lsp-position-line-and-character ()
  (with-temp-buffer
    (insert "line one\nline two\nline three")
    (goto-char (point-min))
    (forward-line 1)
    (forward-char 5)
    (should (equal (list :line 1 :character 5) (ecoin--lsp-position)))))

(ert-deftest ecoin-test-lsp-position-round-trips-through-astral-char ()
  (with-temp-buffer
    (insert "a\U0001F600bc\nsecond line")
    (dolist (pos (list (point-min)
                        (+ (point-min) 1)   ; between "a" and the emoji
                        (+ (point-min) 2)   ; between the emoji and "b"
                        (+ (point-min) 3)   ; between "b" and "c"
                        (+ (point-min) 4)   ; end of the first line
                        (point-max)))
      (should (= pos (ecoin--from-lsp-position (ecoin--lsp-position pos)))))))

;;;; ecoin--language-id / ecoin-major-mode-alist

(ert-deftest ecoin-test-language-id-known-mapping ()
  (with-temp-buffer
    (setq major-mode 'emacs-lisp-mode)
    (should (equal "elisp" (ecoin--language-id)))))

(ert-deftest ecoin-test-language-id-strips-ts-suffix-then-maps ()
  (with-temp-buffer
    (setq major-mode 'tsx-ts-mode)
    (should (equal "typescriptreact" (ecoin--language-id)))))

(ert-deftest ecoin-test-language-id-maps-symbol-heavy-name ()
  (with-temp-buffer
    (setq major-mode 'c++-mode)
    (should (equal "cpp" (ecoin--language-id)))))

(ert-deftest ecoin-test-language-id-falls-back-to-mode-name ()
  ;; Not every mode needs a mapping: "python" is already a valid LSP
  ;; languageId, so `ecoin-major-mode-alist' has no entry for it.
  (with-temp-buffer
    (setq major-mode 'python-mode)
    (should (equal "python" (ecoin--language-id)))))

;;;; ecoin--indent-width

(ert-deftest ecoin-test-indent-width-prefers-language-var-over-standard-indent ()
  (let ((python-indent-offset 7)
        (standard-indent 4))
    (with-temp-buffer
      (should (= 7 (ecoin--indent-width))))))

(ert-deftest ecoin-test-indent-width-falls-back-to-standard-indent ()
  (let ((python-indent-offset nil)
        (standard-indent 9))
    (with-temp-buffer
      (should (= 9 (ecoin--indent-width))))))

(ert-deftest ecoin-test-indent-width-falls-back-to-tab-width ()
  ;; Forcing `standard-indent' non-integer simulates every var in the
  ;; priority list being absent, so the `or' must reach `tab-width'.
  (let ((python-indent-offset nil)
        (standard-indent nil)
        (tab-width 5))
    (with-temp-buffer
      (should (= 5 (ecoin--indent-width))))))

;;;; ecoin-accept-word / ecoin-accept-line

(ert-deftest ecoin-test-accept-word-stops-at-space ()
  (with-temp-buffer
    (ecoin-test--show-ghost "foo bar baz")
    (cl-letf (((symbol-function 'ecoin--notify) #'ignore))
      (ecoin-accept-word))
    (should (equal "foo" (buffer-string)))))

(ert-deftest ecoin-test-accept-word-single-punctuation-char ()
  (with-temp-buffer
    (ecoin-test--show-ghost ".foo")
    (cl-letf (((symbol-function 'ecoin--notify) #'ignore))
      (ecoin-accept-word))
    (should (equal "." (buffer-string)))))

(ert-deftest ecoin-test-accept-word-includes-leading-newline-and-indent ()
  (with-temp-buffer
    (ecoin-test--show-ghost "\n    return value")
    (cl-letf (((symbol-function 'ecoin--notify) #'ignore))
      (ecoin-accept-word))
    (should (equal "\n    return" (buffer-string)))))

(ert-deftest ecoin-test-accept-line-basic ()
  (with-temp-buffer
    (ecoin-test--show-ghost "line one\nline two")
    (cl-letf (((symbol-function 'ecoin--notify) #'ignore))
      (ecoin-accept-line))
    (should (equal "line one" (buffer-string)))))

(ert-deftest ecoin-test-accept-line-includes-leading-blank-line ()
  (with-temp-buffer
    (ecoin-test--show-ghost "\nsecond line\nthird line")
    (cl-letf (((symbol-function 'ecoin--notify) #'ignore))
      (ecoin-accept-line))
    (should (equal "\nsecond line" (buffer-string)))))

;;;; Status notifications
;;
;; Payloads below are copied verbatim from a real copilot-language-server
;; 1.544.0, signed in and signed out.  It sends BOTH shapes for one condition,
;; which is why the handler has to suppress the duplicate.

(defmacro ecoin-test--with-status (&rest body)
  "Run BODY with the status variables reset, returning the messages it emitted."
  (declare (indent 0))
  `(let ((ecoin--status nil)
         (ecoin--status-message nil)
         (ecoin--status-v2 nil)
         (captured '()))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args) (push (apply #'format fmt args) captured))))
       ,@body)
     (nreverse captured)))

(defconst ecoin-test--v2-normal
  '(:statuses [(:category "cls" :kind "Normal" :inactive :json-false)
               (:category "completion" :busy :json-false)]))

(defconst ecoin-test--v2-auth-error
  '(:statuses [(:category "auth" :kind "Error"
                :message "You are not signed into GitHub."
                :askToReSignin :json-false
                :result (:status "NotSignedIn"))]))

(defconst ecoin-test--flat-error
  '(:busy :json-false :kind "Error"
    :message "You are not signed into GitHub."))

(ert-deftest ecoin-test-status-v2-error-is-reported-with-a-login-hint ()
  (let ((msgs (ecoin-test--with-status
                (ecoin--handle-notification nil 'didChangeStatus/v2
                                            ecoin-test--v2-auth-error))))
    (should (= 1 (length msgs)))
    (should (string-match-p "not signed into GitHub" (car msgs)))
    (should (string-match-p "ecoin-login" (car msgs)))))

(ert-deftest ecoin-test-status-is-reported-once-across-both-shapes ()
  "The server sends v2 and the flat form for one condition; report once."
  (let ((msgs (ecoin-test--with-status
                (ecoin--handle-notification nil 'didChangeStatus/v2
                                            ecoin-test--v2-normal)
                (ecoin--handle-notification nil 'didChangeStatus
                                            ecoin-test--flat-error)
                (ecoin--handle-notification nil 'didChangeStatus/v2
                                            ecoin-test--v2-auth-error))))
    (should (= 1 (length msgs)))
    (should (string-match-p "ecoin-login" (car msgs)))))

(ert-deftest ecoin-test-status-flat-form-still-works-without-v2 ()
  "A server that never sends v2 must still surface its errors."
  (let ((msgs (ecoin-test--with-status
                (ecoin--handle-notification nil 'didChangeStatus
                                            ecoin-test--flat-error))))
    (should (= 1 (length msgs)))
    (should (string-match-p "not signed into GitHub" (car msgs)))))

(ert-deftest ecoin-test-status-normal-is-silent-and-clears-the-error ()
  (let ((msgs (ecoin-test--with-status
                (ecoin--handle-notification nil 'didChangeStatus
                                            ecoin-test--flat-error)
                (ecoin--handle-notification nil 'didChangeStatus
                                            '(:kind "Normal" :busy :json-false))
                ;; the same error again, now that it has cleared, reports again
                (ecoin--handle-notification nil 'didChangeStatus
                                            ecoin-test--flat-error))))
    (should (= 2 (length msgs)))))

(ert-deftest ecoin-test-status-v2-records-the-auth-result ()
  (ecoin-test--with-status
    (ecoin--handle-notification nil 'didChangeStatus/v2 ecoin-test--v2-auth-error)
    (should (equal "NotSignedIn"
                   (plist-get (plist-get ecoin--status :result) :status)))))

(ert-deftest ecoin-test-status-v2-without-an-auth-entry-is-ignored ()
  (let ((msgs (ecoin-test--with-status
                (ecoin--handle-notification nil 'didChangeStatus/v2
                                            ecoin-test--v2-normal))))
    (should (null msgs))))

;;;; ecoin--conn readiness and connect backoff

(ert-deftest ecoin-test-conn-withholds-connection-until-ready ()
  "`ecoin--conn' reports nothing until `initialize' has been answered.
Callers on the typing path must stay quiet rather than send `didOpen'
into a server that has not finished its handshake."
  (let ((ecoin--connection 'fake)
        (ecoin--ready nil)
        (ecoin--last-connect-attempt nil))
    (cl-letf (((symbol-function 'jsonrpc-running-p) (lambda (_) t))
              ((symbol-function 'ecoin--connect)
               (lambda () (error "must not reconnect while the process is live"))))
      (should-not (ecoin--conn))
      (setq ecoin--ready t)
      (should (eq 'fake (ecoin--conn))))))

(ert-deftest ecoin-test-conn-backs-off-after-a-failed-connect ()
  "A server that never comes up is not respawned on every idle tick.
Without the backoff this spawned one node process per keystroke, since
`ecoin--conn' no longer blocks on the handshake."
  (let ((ecoin--connection nil)
        (ecoin--ready nil)
        (ecoin--last-connect-attempt nil)
        (calls 0))
    (cl-letf (((symbol-function 'ecoin--connect)
               (lambda () (setq calls (1+ calls)) nil)))
      (dotimes (_ 5) (ecoin--conn))
      (should (= 1 calls))
      ;; ...but it does try again once the backoff has elapsed.
      (setq ecoin--last-connect-attempt
            (- (float-time) (1+ ecoin--connect-backoff)))
      (ecoin--conn)
      (should (= 2 calls)))))

;;;; ecoin--purge-stale-cache

;; The guard matters more than the deletion: this runs against a path in the
;; user's config directory, so it must never remove anything real.

(defmacro ecoin-test--with-fake-cache (&rest body)
  "Run BODY with `ecoin-purge-path-before-connect' inside a temp directory."
  (declare (indent 0))
  `(let* ((tmp (make-temp-file "ecoin-purge" t))
          (ecoin-purge-path-before-connect (expand-file-name "github" tmp)))
     (unwind-protect (progn ,@body)
       (delete-directory tmp t))))

(ert-deftest ecoin-test-purge-removes-empty-directory ()
  (ecoin-test--with-fake-cache
    (make-directory ecoin-purge-path-before-connect t)
    (ecoin--purge-stale-cache)
    (should-not (file-exists-p ecoin-purge-path-before-connect))))

(ert-deftest ecoin-test-purge-removes-nested-but-fileless-directories ()
  ;; What the server actually leaves behind: owner/repo/agents, all empty.
  (ecoin-test--with-fake-cache
    (make-directory (expand-file-name "owner/repo/agents"
                                      ecoin-purge-path-before-connect)
                    t)
    (ecoin--purge-stale-cache)
    (should-not (file-exists-p ecoin-purge-path-before-connect))))

(ert-deftest ecoin-test-purge-removes-a-plain-file ()
  ;; A file at that path wedges the server just as a directory does.
  (ecoin-test--with-fake-cache
    (with-temp-file ecoin-purge-path-before-connect (insert ""))
    (ecoin--purge-stale-cache)
    (should-not (file-exists-p ecoin-purge-path-before-connect))))

(ert-deftest ecoin-test-purge-keeps-a-directory-holding-real-data ()
  (ecoin-test--with-fake-cache
    (let ((deep (expand-file-name "owner/repo" ecoin-purge-path-before-connect)))
      (make-directory deep t)
      (with-temp-file (expand-file-name "index.json" deep) (insert "{}"))
      (ecoin--purge-stale-cache)
      (should (file-exists-p ecoin-purge-path-before-connect))
      (should (file-exists-p (expand-file-name "index.json" deep))))))

(ert-deftest ecoin-test-purge-unlinks-a-symlink-without-touching-its-target ()
  (ecoin-test--with-fake-cache
    (let ((target (expand-file-name
                   "real-data"
                   (file-name-directory ecoin-purge-path-before-connect))))
      (make-directory target t)
      (with-temp-file (expand-file-name "keep.json" target) (insert "{}"))
      (make-symbolic-link target ecoin-purge-path-before-connect)
      (ecoin--purge-stale-cache)
      (should-not (file-exists-p ecoin-purge-path-before-connect))
      (should (file-exists-p (expand-file-name "keep.json" target))))))

(ert-deftest ecoin-test-purge-is-a-noop-when-the-path-is-absent ()
  (ecoin-test--with-fake-cache
    (ecoin--purge-stale-cache)
    (should-not (file-exists-p ecoin-purge-path-before-connect))))

(ert-deftest ecoin-test-purge-respects-the-nil-opt-out ()
  (ecoin-test--with-fake-cache
    (make-directory ecoin-purge-path-before-connect t)
    (let ((kept ecoin-purge-path-before-connect)
          (ecoin-purge-path-before-connect nil))
      (ecoin--purge-stale-cache)
      (should (file-exists-p kept)))))

(provide 'ecoin-test)
;;; ecoin-test.el ends here
