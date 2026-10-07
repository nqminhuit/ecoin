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

;;;; Frontend: evil, popups, exclusions

;; Neither evil nor corfu is installed in CI; these are only declared so the
;; tests can bind them.
(defvar evil-local-mode)
(defvar evil-insert-state-exit-hook)
(defvar corfu--frame)
(defvar company-candidates)
(defvar multiple-cursors-mode)
(defvar so-long-minor-mode)

(defun ecoin-test--show (content ghost &optional pos)
  "In the current buffer insert CONTENT, go to POS (default end), show GHOST."
  (insert content)
  (goto-char (or pos (point-max)))
  (setq ecoin-test--items (list (ecoin-test--item ghost)))
  (ecoin--request 'manual))

(defun ecoin-test--after-string-cursor ()
  "The `cursor' property on char 0 of the ghost's after-string."
  (get-text-property 0 'cursor (overlay-get ecoin--overlay 'after-string)))

(ert-deftest ecoin-test-insert-state-p-without-evil ()
  (with-temp-buffer
    (should (ecoin-insert-state-p))))

(ert-deftest ecoin-test-insert-state-p-follows-the-evil-state ()
  (with-temp-buffer
    (let ((evil-local-mode t) (insert nil) (emacs nil))
      (cl-letf (((symbol-function 'evil-insert-state-p) (lambda () insert))
                ((symbol-function 'evil-emacs-state-p) (lambda () emacs)))
        (should-not (ecoin-insert-state-p))
        (setq emacs t)
        (should (ecoin-insert-state-p))
        (setq emacs nil insert t)
        (should (ecoin-insert-state-p))))))

(ert-deftest ecoin-test-no-automatic-request-outside-insert-state ()
  (ecoin-test--with-buffer "foo"
    (let ((evil-local-mode t))
      (cl-letf (((symbol-function 'evil-insert-state-p) (lambda () nil))
                ((symbol-function 'evil-emacs-state-p) (lambda () nil)))
        (should-not (ecoin--allowed-p))))))

(ert-deftest ecoin-test-reply-after-leaving-insert-state-is-dropped ()
  (ecoin-test--with-buffer "foo"
    (setq ecoin-test--defer t)
    (ecoin--request 'auto)
    (let ((evil-local-mode t))
      (cl-letf (((symbol-function 'evil-insert-state-p) (lambda () nil))
                ((symbol-function 'evil-emacs-state-p) (lambda () nil)))
        (funcall (car ecoin-test--callbacks) (list (ecoin-test--item "x")))))
    (should-not (ecoin-test--ghost))))

(ert-deftest ecoin-test-evil-insert-exit-hook-hides-the-ghost ()
  (let ((evil-insert-state-exit-hook nil))
    (ecoin-test--with-buffer ""
      (ecoin-test--show "foo" "bar")
      (should (ecoin-test--ghost))
      (should (memq #'ecoin--hide (buffer-local-value 'evil-insert-state-exit-hook
                                                      (current-buffer))))
      (run-hooks 'evil-insert-state-exit-hook)
      (should-not (ecoin-test--ghost)))))

(ert-deftest ecoin-test-overlay-has-the-keymap-window-and-priority ()
  (ecoin-test--with-buffer ""
    (let ((ecoin-overlay-priority 77))
      (ecoin-test--show "foo" "bar"))
    (should (eq ecoin-completion-map (get-pos-property (point) 'keymap)))
    (should (eq (selected-window) (overlay-get ecoin--overlay 'window)))
    (should (= 77 (overlay-get ecoin--overlay 'priority)))
    (should (= 101 (overlay-get ecoin--keymap-overlay 'priority)))
    (should (eq 'ecoin-accept (lookup-key ecoin-completion-map (kbd "TAB"))))
    (should-not (memq 'ecoin--emulation-alist emulation-mode-map-alists))))

(ert-deftest ecoin-test-keymap-overlay-is-zero-length-at-the-end-of-the-buffer ()
  (ecoin-test--with-buffer ""
    (ecoin-test--show "foo" "bar")
    (should (= 0 (- (overlay-end ecoin--keymap-overlay)
                    (overlay-start ecoin--keymap-overlay))))
    (ecoin-test--show "" "baz" 2)
    (should (= 1 (- (overlay-end ecoin--keymap-overlay)
                    (overlay-start ecoin--keymap-overlay))))))

(ert-deftest ecoin-test-clearing-removes-both-overlays ()
  (ecoin-test--with-buffer ""
    (ecoin-test--show "foo" "bar")
    (ecoin-dismiss)
    (should (null (overlays-in (point-min) (1+ (point-max)))))
    (should-not ecoin--overlay-active)))

(ert-deftest ecoin-test-cursor-property-is-t-at-eol-and-1-mid-line ()
  (ecoin-test--with-buffer ""
    (ecoin-test--show "foo" "bar")
    (should (eq t (ecoin-test--after-string-cursor)))
    (ecoin-test--show "" "baz" 2)
    (should (eql 1 (ecoin-test--after-string-cursor)))))

(ert-deftest ecoin-test-no-display-property-is-used ()
  (ecoin-test--with-buffer ""
    (ecoin-test--show "foo" "bar" 2)
    (should-not (overlay-get ecoin--overlay 'display))
    (should (= (overlay-start ecoin--overlay) (overlay-end ecoin--overlay)))
    (should (equal "foo" (buffer-string)))))

(ert-deftest ecoin-test-leading-newline-gets-a-space-for-the-cursor ()
  (ecoin-test--with-buffer ""
    (ecoin-test--show "def f():" "\n    return 1")
    (let ((str (overlay-get ecoin--overlay 'after-string)))
      (should (equal " \n    return 1" (substring-no-properties str)))
      (should (eq t (get-text-property 0 'cursor str))))
    (should (equal "\n    return 1" (ecoin-test--ghost)))))

(ert-deftest ecoin-test-pre-command-hides-the-ghost-for-motion ()
  (ecoin-test--with-buffer ""
    (ecoin-test--show "foo" "bar")
    (let ((this-command 'next-line) (this-original-command 'next-line))
      (ecoin--pre-command))
    (should-not (ecoin-test--ghost))))

(ert-deftest ecoin-test-pre-command-keeps-the-ghost-for-typing-ecoin-and-prefix-commands ()
  (dolist (cmd '(self-insert-command ecoin-accept ecoin-next ecoin-complete
                 universal-argument digit-argument negative-argument
                 universal-argument-more))
    (ecoin-test--with-buffer ""
      (ecoin-test--show "foo" "bar")
      (let ((this-command cmd) (this-original-command cmd))
        (ecoin--pre-command))
      (should (ecoin-test--ghost)))))

(ert-deftest ecoin-test-prefix-command-keeps-the-ghost-in-post-command ()
  (ecoin-test--with-buffer ""
    (ecoin-test--show "foo" "bar")
    (let ((this-command 'universal-argument-more)
          (this-original-command 'universal-argument))
      (ecoin--post-command))
    (should (ecoin-test--ghost))
    (let ((this-command 'next-line) (this-original-command 'next-line))
      (ecoin--post-command))
    (should-not (ecoin-test--ghost))))

(ert-deftest ecoin-test-typing-the-last-ghost-char-accepts-it ()
  (ecoin-test--with-buffer ""
    (ecoin-test--show "fo" "ob")
    (let ((item (overlay-get ecoin--overlay 'ecoin-item))
          (this-command 'self-insert-command))
      (insert "o")
      (should (ecoin--typed-into-ghost))
      (should (equal "b" (ecoin-test--ghost)))
      (insert "b")
      (should (ecoin--typed-into-ghost))
      (should-not (ecoin-test--ghost))
      (should (= 1 (length ecoin-test--accepted)))
      (should (equal '("b" nil) (cdar ecoin-test--accepted)))
      (should (eq 'ecoin-test-stub (ecoin-item-backend (caar ecoin-test--accepted))))
      (should (eq 'ecoin-test-stub (ecoin-item-backend item))))
    (should-not ecoin--timer)))

(ert-deftest ecoin-test-trigger-on-move-needs-the-capability ()
  (dolist (cap '(nil t))
    (cl-letf (((symbol-function 'ecoin-backend-capabilities)
               (lambda (_backend) (and cap '(:trigger-on-move t)))))
      (ecoin-test--with-buffer "foo bar"
        (ecoin--post-command)
        (goto-char 2)
        (ecoin--post-command)
        (should (eq (and cap t) (and ecoin--timer t)))
        (ecoin--cancel-timer)))))

(ert-deftest ecoin-test-an-edit-schedules-a-request-without-the-capability ()
  (ecoin-test--with-buffer "foo"
    (insert "d")
    (ecoin--post-command)
    (should ecoin--timer)
    (ecoin--cancel-timer)))

(ert-deftest ecoin-test-exclusion-regexps ()
  (dolist (file '("/p/.env" "/p/.env.local" "/home/u/.authinfo" "/home/u/.authinfo.gpg"
                  "/home/u/.netrc" "/p/a.gpg" "/p/a.age" "/p/cert.pem" "/p/server.key"
                  "/home/u/.ssh/config" "/home/u/id_rsa" "/home/u/id_ed25519.pub"
                  "/home/u/.aws/credentials" "/home/u/.gnupg/gpg.conf"
                  "/home/u/.password-store/a.txt" "/p/secrets.yml" "/p/secrets.yaml"
                  "/p/secrets.json" "/p/secrets.env"))
    (with-temp-buffer
      (setq buffer-file-name file)
      (should (ecoin--excluded-p))))
  (dolist (file '("/p/main.py" "/p/environment.el" "/p/env.el" "/p/keymap.el"
                  "/p/monkey.el" "/p/secrets.md" "/p/netrc.el" "/p/foo.agent"))
    (with-temp-buffer
      (setq buffer-file-name file)
      (should-not (ecoin--excluded-p))))
  (with-temp-buffer
    (should-not (ecoin--excluded-p))))

(ert-deftest ecoin-test-exclude-functions ()
  (with-temp-buffer
    (let ((ecoin-exclude-functions (list (lambda () t))))
      (should (ecoin--excluded-p)))
    (let ((ecoin-exclude-functions (list (lambda () (error "boom")))))
      (should (ecoin--excluded-p)))))

(ert-deftest ecoin-test-excluded-buffer-never-requests ()
  (ecoin-test--with-buffer "foo"
    (setq buffer-file-name "/p/.env")
    (unwind-protect
        (progn
          (should-not (ecoin--allowed-p))
          (setq ecoin-test--items (list (ecoin-test--item "bar")))
          (let (msg)
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
              (ecoin-complete))
            (should (string-match-p "excluded" msg)))
          (should-not (ecoin-test--ghost))
          (should-not ecoin-test--shown))
      (setq buffer-file-name nil))))

(ert-deftest ecoin-test-completion-in-region-gates-requests-and-hides-the-ghost ()
  (ecoin-test--with-buffer ""
    (ecoin-test--show "foo" "bar")
    (should (ecoin--allowed-p))
    (setq-local completion-in-region-mode t)
    (unwind-protect
        (progn
          (should-not (ecoin--allowed-p))
          (let ((this-command 'self-insert-command))
            (ecoin--post-command))
          (should-not (ecoin-test--ghost))
          (should-not ecoin--timer))
      (kill-local-variable 'completion-in-region-mode))))

(ert-deftest ecoin-test-corfu-frame-gates-only-while-visible ()
  (ecoin-test--with-buffer "foo"
    (let ((corfu--frame 'frame))
      (cl-letf (((symbol-function 'frame-live-p) (lambda (f) (eq f 'frame)))
                ((symbol-function 'frame-visible-p) (lambda (_f) nil)))
        (should (ecoin--allowed-p)))
      (cl-letf (((symbol-function 'frame-live-p) (lambda (f) (eq f 'frame)))
                ((symbol-function 'frame-visible-p) (lambda (_f) t)))
        (should-not (ecoin--allowed-p))))))

(ert-deftest ecoin-test-other-quiet-situations ()
  (ecoin-test--with-buffer "foo"
    (should (ecoin--allowed-p))
    (let ((executing-kbd-macro t)) (should-not (ecoin--allowed-p)))
    (let ((company-candidates '("a"))) (should-not (ecoin--allowed-p)))
    (let ((multiple-cursors-mode t)) (should-not (ecoin--allowed-p)))
    (let ((so-long-minor-mode t)) (should-not (ecoin--allowed-p)))
    (cl-letf (((symbol-function 'invisible-p) (lambda (_p) t)))
      (should-not (ecoin--allowed-p)))
    (cl-letf (((symbol-function 'evil-mc-has-cursors-p) (lambda () t)))
      (should-not (ecoin--allowed-p)))))

(ert-deftest ecoin-test-disable-predicates-still-work ()
  (ecoin-test--with-buffer "foo"
    (let ((ecoin-disable-predicates (list (lambda () t))))
      (should-not (ecoin--allowed-p)))))

(ert-deftest ecoin-test-accept-drops-indentation-already-before-point ()
  (ecoin-test--with-buffer ""
    (ecoin-test--show "def f():\n    " "    return 1")
    (ecoin-accept)
    (should (equal "def f():\n    return 1" (buffer-string)))
    (should (equal "return 1" (cadar ecoin-test--accepted)))))

(ert-deftest ecoin-test-accept-keeps-extra-indentation ()
  (ecoin-test--with-buffer ""
    (ecoin-test--show "if x:\n    " "        y")
    (ecoin-accept)
    (should (equal "if x:\n        y" (buffer-string)))))

(ert-deftest ecoin-test-accept-does-not-dedent-after-text ()
  (ecoin-test--with-buffer ""
    (ecoin-test--show "foo" "  bar")
    (ecoin-accept)
    (should (equal "foo  bar" (buffer-string)))))

(ert-deftest ecoin-test-accept-word-dedents-and-keeps-the-remainder ()
  (ecoin-test--with-buffer ""
    (ecoin-test--show "    " "    return 1")
    (ecoin-accept-word)
    (should (equal "    return" (buffer-string)))
    (should (equal " 1" (ecoin-test--ghost)))))

;; Dedent happens on delivery, so the ghost is what accepting inserts.
(ert-deftest ecoin-test-delivered-text-is-dedented-only-in-indentation ()
  (ecoin-test--with-buffer ""
    (ecoin-test--show "def f():\n    " "    return x")
    (should (equal "return x" (ecoin-test--ghost))))
  (ecoin-test--with-buffer ""
    (ecoin-test--show "foo" "  bar")
    (should (equal "  bar" (ecoin-test--ghost)))))

(ert-deftest ecoin-test-displayed-ghost-equals-the-accepted-insertion ()
  (ecoin-test--with-buffer ""
    (ecoin-test--show "def f():\n    " "    return x")
    (let ((shown (substring-no-properties
                  (overlay-get ecoin--overlay 'after-string))))
      (ecoin-accept)
      (should (equal shown (cadar ecoin-test--accepted)))
      (should (equal "def f():\n    return x" (buffer-string))))))

(ert-deftest ecoin-test-item-that-is-only-duplicated-indentation-is-dropped ()
  (ecoin-test--with-buffer ""
    (ecoin-test--show "if x:\n    " "    ")
    (should-not (ecoin-test--ghost))
    (should-not ecoin-test--shown)))

(ert-deftest ecoin-test-log-keeps-buffer-text-out-unless-asked ()
  (cl-letf (((symbol-function 'ecoin-backend-accepted)
             (lambda (&rest _) (signal 'args-out-of-range '("secret buffer text" 1)))))
    (dolist (case '((nil . nil) (t . t)))
      (when (get-buffer "*ecoin-log*") (kill-buffer "*ecoin-log*"))
      (let ((ecoin-log-content (car case)))
        (ecoin--hook #'ecoin-backend-accepted 'ecoin-test-stub nil "x" nil))
      (with-current-buffer "*ecoin-log*"
        (should (eq (cdr case) (and (string-match-p "secret buffer text" (buffer-string)) t)))
        (should (string-match-p "args-out-of-range\\|Args out of range" (buffer-string)))))))

;;;; Fallback

(defvar ecoin-test--primary-up t "Whether the primary stub reports itself available.")
(defvar ecoin-test--requests nil "Backends that were sent a request, newest first.")
(defvar ecoin-test--closed nil "Backends told a buffer closed, newest first.")
(defvar ecoin-test--messages nil "Messages shown, newest first.")

(cl-defmethod ecoin-backend-request ((backend (eql 'ecoin-test-primary)) _r callback)
  (push backend ecoin-test--requests)
  (funcall callback (list (ecoin-make-item :text "from-primary")))
  nil)

(cl-defmethod ecoin-backend-request ((backend (eql 'ecoin-test-fallback)) _r callback)
  (push backend ecoin-test--requests)
  (funcall callback (list (ecoin-make-item :text "from-fallback")))
  nil)

(cl-defmethod ecoin-backend-available-p ((_b (eql 'ecoin-test-primary)))
  ecoin-test--primary-up)

(cl-defmethod ecoin-backend-unavailable-reason ((_b (eql 'ecoin-test-primary)))
  "down")

(cl-defmethod ecoin-backend-shown ((backend (eql 'ecoin-test-fallback)) _item)
  (push (cons 'shown backend) ecoin-test--closed))

(cl-defmethod ecoin-backend-accepted ((backend (eql 'ecoin-test-fallback)) _i _t _p)
  (push (cons 'accepted backend) ecoin-test--closed))

(cl-defmethod ecoin-backend-accepted ((backend (eql 'ecoin-test-primary)) _i _t _p)
  (push (cons 'accepted backend) ecoin-test--closed))

(cl-defmethod ecoin-backend-disable-buffer ((backend (eql 'ecoin-test-primary)))
  (push (cons 'closed backend) ecoin-test--closed))

(cl-defmethod ecoin-backend-disable-buffer ((backend (eql 'ecoin-test-fallback)))
  (push (cons 'closed backend) ecoin-test--closed))

(defmacro ecoin-test--with-fallback (content &rest body)
  "Run BODY in a buffer of CONTENT, primary and fallback stubs configured."
  (declare (indent 1))
  `(let ((ecoin-backend 'ecoin-test-primary)
         (ecoin-fallback-backend 'ecoin-test-fallback)
         (ecoin-test--primary-up t) (ecoin-test--requests nil)
         (ecoin-test--closed nil) (ecoin-test--messages nil)
         (ecoin--fallback-state nil))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (when fmt (push (apply #'format fmt args) ecoin-test--messages)))))
       (with-temp-buffer
         (insert ,content)
         (ecoin-mode 1)
         (unwind-protect (progn ,@body)
           (ecoin-mode -1))))))

(ert-deftest ecoin-test-fallback-switches-with_one_message_each_way ()
  (ecoin-test--with-fallback "foo"
    (ecoin--request 'manual)
    (should (equal "from-primary" (ecoin-test--ghost)))
    (should-not ecoin-test--messages)
    (setq ecoin-test--primary-up nil)
    (ecoin--request 'manual)
    (ecoin--request 'manual)
    (should (equal "from-fallback" (ecoin-test--ghost)))
    (should (equal '("ecoin: ecoin-test-primary unavailable (down); using ecoin-test-fallback")
                   ecoin-test--messages))
    (should (equal " ecoin[ecoin-test-fallback]" (ecoin--lighter)))
    (setq ecoin-test--primary-up t)
    (ecoin--request 'manual)
    (ecoin--request 'manual)
    (should (equal "from-primary" (ecoin-test--ghost)))
    (should (equal "ecoin: ecoin-test-primary is back" (car ecoin-test--messages)))
    (should (= 2 (length ecoin-test--messages)))
    (should (equal " ecoin" (ecoin--lighter)))))

(ert-deftest ecoin-test-fallback-nil-or-equal-to-primary-never-falls-back ()
  (dolist (fallback '(nil ecoin-test-primary))
    (ecoin-test--with-fallback "foo"
      (let ((ecoin-fallback-backend fallback))
        (setq ecoin-test--primary-up nil)
        (ecoin--request 'manual)
        (should (equal "from-primary" (ecoin-test--ghost)))
        (should (equal '(ecoin-test-primary) ecoin-test--requests))
        (should-not ecoin-test--messages)))))

(ert-deftest ecoin-test-fallback-lighter-marks-copilot ()
  (let ((ecoin-backend 'ecoin-test-primary) (ecoin-fallback-backend 'copilot)
        (ecoin--fallback-state '(ecoin-test-primary . copilot)))
    (should (equal " ecoin[cp]" (ecoin--lighter)))))

(ert-deftest ecoin-test-fallback-skips-excluded-buffers ()
  (ecoin-test--with-fallback "foo"
    (setq ecoin-test--primary-up nil)
    (let ((ecoin-exclude-functions (list (lambda () t))))
      (ecoin--request 'manual)
      (ecoin-complete)
      (should-not ecoin-test--requests)
      (should-not (seq-filter (lambda (m) (string-match-p "unavailable" m))
                              ecoin-test--messages))
      (should-not (ecoin-test--ghost)))))

(ert-deftest ecoin-test-fallback-hooks-reach-the-items-own-backend ()
  (ecoin-test--with-fallback "foo"
    (setq ecoin-test--primary-up nil)
    (ecoin--request 'manual)
    (setq ecoin-test--primary-up t)
    (ecoin-accept)
    (should (equal '((accepted . ecoin-test-fallback) (shown . ecoin-test-fallback))
                   (seq-filter (lambda (e) (memq (car e) '(shown accepted)))
                               ecoin-test--closed)))))

(ert-deftest ecoin-test-disable-buffer-reaches-every-serving-backend ()
  (ecoin-test--with-fallback "foo"
    (ecoin--request 'manual)
    (setq ecoin-test--primary-up nil)
    (ecoin--request 'manual)
    (setq ecoin-test--primary-up t)
    (ecoin-mode -1)
    (let ((closed (mapcar #'cdr (seq-filter (lambda (e) (eq (car e) 'closed))
                                            ecoin-test--closed))))
      (should (= 2 (length closed)))
      (should (memq 'ecoin-test-primary closed))
      (should (memq 'ecoin-test-fallback closed)))))

(ert-deftest ecoin-test-status-reports-fallback-without-starting-it ()
  (let ((ecoin-backend 'ecoin-test-stub) (ecoin-fallback-backend 'copilot)
        (ecoin--fallback-state nil) msg)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (ecoin-status))
    (should (equal "ecoin: stub is fine; fallback copilot: not started" msg))
    (should-not (featurep 'ecoin-copilot))))

(provide 'ecoin-test)
;;; ecoin-test.el ends here
