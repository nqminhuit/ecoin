;;; ecoin-test.el --- Tests for the ecoin core -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercises the core through a stub backend, a test symbol with its own
;; `cl-defmethod's, so nothing here needs a server, the network or the
;; Copilot backend.  Run this file in its own Emacs process: the
;; "Copilot is not loaded" test fails if ecoin-copilot.el was loaded earlier.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ecoin)

(defvar ecoin-test--items nil "Items the stub backend replies with.")
(defvar ecoin-test--defer nil "Non-nil: the stub keeps its callbacks instead of calling them.")
(defvar ecoin-test--callbacks nil "Callbacks held back by the stub, oldest first.")
(defvar ecoin-test--shown nil "Items the stub was told were shown.")
(defvar ecoin-test--accepted nil "(ITEM TEXT PARTIAL) for each accept the stub was told about.")
(defvar ecoin-test--cancelled nil "Handles the stub was asked to cancel.")

(cl-defmethod ecoin-backend-request ((_backend (eql 'ecoin-test-stub)) _request callback)
  (if ecoin-test--defer
      (setq ecoin-test--callbacks (append ecoin-test--callbacks (list callback)))
    (funcall callback ecoin-test--items))
  'handle)

(cl-defmethod ecoin-backend-cancel ((_backend (eql 'ecoin-test-stub)) handle)
  (push handle ecoin-test--cancelled))

(cl-defmethod ecoin-backend-shown ((_backend (eql 'ecoin-test-stub)) item)
  (push item ecoin-test--shown))

(cl-defmethod ecoin-backend-accepted ((_backend (eql 'ecoin-test-stub)) item text partial)
  (push (list item text partial) ecoin-test--accepted))

(cl-defmethod ecoin-backend-status ((_backend (eql 'ecoin-test-stub)))
  "stub is fine")

(defun ecoin-test--item (text &optional end)
  "A stub item inserting TEXT, replacing up to END."
  (ecoin-make-item :text text :end end :backend 'ecoin-test-stub))

(defmacro ecoin-test--with-buffer (content &rest body)
  "Run BODY in a buffer holding CONTENT, point at its end, with `ecoin-mode' on."
  (declare (indent 1))
  `(let ((ecoin-backend 'ecoin-test-stub)
         (ecoin-test--items nil) (ecoin-test--defer nil) (ecoin-test--callbacks nil)
         (ecoin-test--shown nil) (ecoin-test--accepted nil) (ecoin-test--cancelled nil))
     (with-temp-buffer
       (insert ,content)
       (ecoin-mode 1)
       (unwind-protect (progn ,@body)
         (ecoin-mode -1)))))

(defun ecoin-test--ghost ()
  "The ghost text on display, or nil."
  (and (ecoin--visible-p) (ecoin--overlay-ghost)))

;;;; Showing

(ert-deftest ecoin-test-request-shows-the-item-as-ghost-text ()
  (ecoin-test--with-buffer "foo"
    (setq ecoin-test--items (list (ecoin-test--item "bar")))
    (ecoin--request 'manual)
    (should (equal "bar" (ecoin-test--ghost)))
    (should (equal "foo" (buffer-string)))
    (should ecoin--overlay-active)))

(ert-deftest ecoin-test-no-items-clears-the-ghost ()
  (ecoin-test--with-buffer "foo"
    (setq ecoin-test--items (list (ecoin-test--item "bar")))
    (ecoin--request 'manual)
    (setq ecoin-test--items nil)
    (ecoin--request 'manual)
    (should-not (ecoin-test--ghost))))

(ert-deftest ecoin-test-empty-text-items-are-not-shown ()
  (ecoin-test--with-buffer "foo"
    (setq ecoin-test--items (list (ecoin-test--item "")))
    (ecoin--request 'manual)
    (should-not (ecoin-test--ghost))
    (should-not ecoin-test--shown)))

(ert-deftest ecoin-test-synchronous-callback-is-handled ()
  "The callback runs inside the backend's request, before it returns."
  (ecoin-test--with-buffer "foo"
    (setq ecoin-test--items (list (ecoin-test--item "bar")))
    (ecoin--request 'auto)
    (should (equal "bar" (ecoin-test--ghost)))
    (should (null ecoin--pending))))

(ert-deftest ecoin-test-deferred-callback-is-handled ()
  (ecoin-test--with-buffer "foo"
    (setq ecoin-test--defer t)
    (ecoin--request 'auto)
    (should-not (ecoin-test--ghost))
    (should ecoin--pending)
    (funcall (car ecoin-test--callbacks) (list (ecoin-test--item "bar")))
    (should (equal "bar" (ecoin-test--ghost)))
    (should (null ecoin--pending))))

(ert-deftest ecoin-test-shown-is-called-once-per-item-and-on-the-items-backend ()
  (ecoin-test--with-buffer "foo"
    (let ((item (ecoin-test--item "bar")))
      (setq ecoin-test--items (list item))
      (ecoin--request 'manual)
      (should (equal (list item) ecoin-test--shown)))))

(ert-deftest ecoin-test-an-item-without-a-backend-gets-the-requested-one ()
  (ecoin-test--with-buffer "foo"
    (setq ecoin-test--items (list (ecoin-make-item :text "bar")))
    (ecoin--request 'manual)
    (should (eq 'ecoin-test-stub
                (ecoin-item-backend (overlay-get ecoin--overlay 'ecoin-item))))))

(ert-deftest ecoin-test-next-and-previous-cycle-and-report-each-show ()
  (ecoin-test--with-buffer "foo"
    (setq ecoin-test--items (list (ecoin-test--item "one") (ecoin-test--item "two")))
    (ecoin--request 'manual)
    (ecoin-next)
    (should (equal "two" (ecoin-test--ghost)))
    (ecoin-next)
    (should (equal "one" (ecoin-test--ghost)))
    (ecoin-previous)
    (should (equal "two" (ecoin-test--ghost)))
    (should (= 4 (length ecoin-test--shown)))))

;;;; Stale replies

(ert-deftest ecoin-test-reply-after-point-moved-is-dropped ()
  (ecoin-test--with-buffer "foo bar"
    (setq ecoin-test--defer t)
    (ecoin--request 'auto)
    (goto-char (point-min))
    (funcall (car ecoin-test--callbacks) (list (ecoin-test--item "x")))
    (should-not (ecoin-test--ghost))
    (should-not ecoin-test--shown)))

(ert-deftest ecoin-test-reply-after-an-edit-is-dropped ()
  (ecoin-test--with-buffer "foo"
    (setq ecoin-test--defer t)
    (ecoin--request 'auto)
    (insert "d")
    (funcall (car ecoin-test--callbacks) (list (ecoin-test--item "x")))
    (should-not (ecoin-test--ghost))))

(ert-deftest ecoin-test-reply-to-a-superseded-request-is-dropped ()
  (ecoin-test--with-buffer "foo"
    (setq ecoin-test--defer t)
    (ecoin--request 'auto)
    (ecoin--request 'auto)
    (funcall (car ecoin-test--callbacks) (list (ecoin-test--item "old")))
    (should-not (ecoin-test--ghost))
    (funcall (cadr ecoin-test--callbacks) (list (ecoin-test--item "new")))
    (should (equal "new" (ecoin-test--ghost)))))

(ert-deftest ecoin-test-a-new-request-cancels-the-one-in-flight ()
  (ecoin-test--with-buffer "foo"
    (setq ecoin-test--defer t)
    (ecoin--request 'auto)
    (ecoin--request 'auto)
    (should (equal '(handle) ecoin-test--cancelled))))

(ert-deftest ecoin-test-reply-for-a-killed-buffer-is-dropped ()
  (let ((ecoin-backend 'ecoin-test-stub) (ecoin-test--defer t)
        (ecoin-test--callbacks nil) (ecoin-test--shown nil)
        (buf (generate-new-buffer " *ecoin-test*")))
    (with-current-buffer buf
      (ecoin-mode 1)
      (ecoin--request 'auto))
    (kill-buffer buf)
    (funcall (car ecoin-test--callbacks) (list (ecoin-test--item "x")))
    (should-not ecoin-test--shown)))

(ert-deftest ecoin-test-reply-after-the-mode-was-turned-off-is-dropped ()
  (ecoin-test--with-buffer "foo"
    (setq ecoin-test--defer t)
    (ecoin--request 'auto)
    (ecoin-mode -1)
    (funcall (car ecoin-test--callbacks) (list (ecoin-test--item "x")))
    (should-not (ecoin-test--ghost))))

;;;; Accepting

(ert-deftest ecoin-test-accept-inserts-everything-and-reports-a-full-accept ()
  (ecoin-test--with-buffer "foo"
    (let ((item (ecoin-test--item " bar baz")))
      (setq ecoin-test--items (list item))
      (ecoin--request 'manual)
      (ecoin-accept)
      (should (equal "foo bar baz" (buffer-string)))
      (should (equal (list (list item " bar baz" nil)) ecoin-test--accepted))
      (should-not (ecoin-test--ghost)))))

(ert-deftest ecoin-test-accept-replaces-up-to-the-items-end ()
  (ecoin-test--with-buffer "ab XYZ"
    (goto-char 4)
    (setq ecoin-test--items (list (ecoin-test--item "hello" (point-max))))
    (ecoin--request 'manual)
    (ecoin-accept)
    (should (equal "ab hello" (buffer-string)))))

(ert-deftest ecoin-test-accept-word-stops-at-space-and-keeps-the-rest ()
  (ecoin-test--with-buffer ""
    (let ((item (ecoin-test--item "foo bar baz")))
      (setq ecoin-test--items (list item))
      (ecoin--request 'manual)
      (ecoin-accept-word)
      (should (equal "foo" (buffer-string)))
      (should (equal " bar baz" (ecoin-test--ghost)))
      (should (equal (list (list item "foo" t)) ecoin-test--accepted)))))

(ert-deftest ecoin-test-accept-word-single-punctuation-char ()
  (ecoin-test--with-buffer ""
    (setq ecoin-test--items (list (ecoin-test--item ".foo")))
    (ecoin--request 'manual)
    (ecoin-accept-word)
    (should (equal "." (buffer-string)))))

(ert-deftest ecoin-test-accept-word-includes-leading-newline-and-indent ()
  (ecoin-test--with-buffer ""
    (setq ecoin-test--items (list (ecoin-test--item "\n    return value")))
    (ecoin--request 'manual)
    (ecoin-accept-word)
    (should (equal "\n    return" (buffer-string)))))

(ert-deftest ecoin-test-accept-line-takes-one-line ()
  (ecoin-test--with-buffer ""
    (setq ecoin-test--items (list (ecoin-test--item "line one\nline two")))
    (ecoin--request 'manual)
    (ecoin-accept-line)
    (should (equal "line one" (buffer-string)))
    (should (equal (list t) (mapcar #'caddr ecoin-test--accepted)))))

(ert-deftest ecoin-test-accept-line-includes-leading-blank-line ()
  (ecoin-test--with-buffer ""
    (setq ecoin-test--items (list (ecoin-test--item "\nsecond line\nthird line")))
    (ecoin--request 'manual)
    (ecoin-accept-line)
    (should (equal "\nsecond line" (buffer-string)))))

(ert-deftest ecoin-test-partial-then-full-accept-reports-partial-then-full ()
  (ecoin-test--with-buffer "ab XYZ"
    (goto-char 4)
    (setq ecoin-test--items (list (ecoin-test--item "foo bar" (point-max))))
    (ecoin--request 'manual)
    (ecoin-accept-word)
    (should (equal "ab fooXYZ" (buffer-string)))
    (ecoin-accept)
    (should (equal "ab foo bar" (buffer-string)))
    (should (equal '(nil t) (mapcar #'caddr ecoin-test--accepted)))
    (should (equal '(" bar" "foo") (mapcar #'cadr ecoin-test--accepted)))
    ;; Following a partial accept is not a new show.
    (should (= 1 (length ecoin-test--shown)))))

(ert-deftest ecoin-test-accept-without-a-suggestion-is-a-user-error ()
  (ecoin-test--with-buffer "foo"
    (should-error (ecoin-accept) :type 'user-error)))

(ert-deftest ecoin-test-typing-the-next-ghost-char-shrinks-it-without-a-new-show ()
  (ecoin-test--with-buffer "fo"
    (setq ecoin-test--items (list (ecoin-test--item "obar")))
    (ecoin--request 'manual)
    (let ((this-command 'self-insert-command))
      (insert "o")
      (should (ecoin--typed-into-ghost)))
    (should (equal "bar" (ecoin-test--ghost)))
    (should (= 1 (length ecoin-test--shown)))))

;;;; Dispatch and lazy loading

(ert-deftest ecoin-test-status-dispatches-to-the-backend ()
  (let ((ecoin-backend 'ecoin-test-stub) msg)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (ecoin-status))
    (should (equal "ecoin: stub is fine" msg))))

(ert-deftest ecoin-test-defaults-are-harmless ()
  (should (null (ecoin-backend-capabilities 'ecoin-test-stub)))
  (should (ecoin-backend-available-p 'ecoin-test-stub))
  (should (null (ecoin-backend-restart 'ecoin-test-stub)))
  (should (null (ecoin-backend-shown 'ecoin-test-other nil)))
  (should (null (ecoin-backend-accepted 'ecoin-test-other nil "x" nil))))

(ert-deftest ecoin-test-request-to-an-unknown-backend-is-reported-not-signalled ()
  (let ((ecoin-backend 'ecoin-test-nonexistent) msg)
    (with-temp-buffer
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
        (ecoin--request 'manual)))
    (should (string-match-p "no backend" msg))))

(ert-deftest ecoin-test-copilot-is-not-loaded-by-a-stub-backend ()
  (ecoin-test--with-buffer "foo"
    (setq ecoin-test--items (list (ecoin-test--item "bar")))
    (ecoin--request 'manual)
    (ecoin-accept)
    (should-not (featurep 'ecoin-copilot))
    (should-not (featurep 'jsonrpc))))

(provide 'ecoin-test)
;;; ecoin-test.el ends here
