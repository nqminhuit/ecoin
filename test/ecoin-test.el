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

(provide 'ecoin-test)
;;; ecoin-test.el ends here
