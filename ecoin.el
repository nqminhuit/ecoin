;;; ecoin.el --- Emacs Copilot INline completions  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, completion
;; URL: https://github.com/nqminhuit/ecoin

;;; Commentary:

;; ecoin shows GitHub Copilot inline suggestions (ghost text) and nothing else.
;; It talks to the official `copilot-language-server' over JSON-RPC using the
;; built-in `jsonrpc' library.  One server process is shared by all buffers.
;;
;; Setup:
;;   npm install -g @github/copilot-language-server
;;   (require 'ecoin)
;;   (add-hook 'prog-mode-hook #'ecoin-mode)
;;   M-x ecoin-login          ; first time only
;;
;; While a suggestion is visible: TAB accepts it, C-TAB accepts one word,
;; M-n / M-p cycle alternatives, any other key dismisses it.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'jsonrpc)
(require 'url-util)
(require 'project)

;;;; Customization

(defgroup ecoin nil
  "Copilot inline completions."
  :group 'completion
  :prefix "ecoin-")

(defcustom ecoin-server-command '("copilot-language-server" "--stdio")
  "Command (program and args) that starts the Copilot language server."
  :type '(repeat string))

(defcustom ecoin-purge-path-before-connect
  (expand-file-name "github-copilot/github"
                    (or (getenv "XDG_CONFIG_HOME") "~/.config"))
  "Path deleted just before the server starts, or nil to leave it alone.

Workaround for an upstream hang.  If this path exists when
`copilot-language-server' starts, the server answers `initialize' and
then never replies to anything again: its main thread parks on a futex
at zero CPU and the process has to be SIGKILLed.  Reproduced without
Emacs on server versions 1.506.1, 1.518.3, 1.532.6, 1.543.0 and 1.544.0,
with the path as a directory or as a plain file, empty or not.

Only the state at startup matters -- the server recreates the directory
during normal use and that does no harm until the next start, which is
why deleting it here is enough.

Deleted only when it holds no files, so if a later server version really
does cache something there, its data is left alone."
  :type '(choice (const :tag "Leave it alone" nil) directory))

(defcustom ecoin-idle-delay 0.15
  "Seconds of idle time after an edit before asking for a suggestion."
  :type 'number)

(defcustom ecoin-max-chars 100000
  "Do not request suggestions in buffers larger than this many characters."
  :type 'integer)

(defcustom ecoin-disable-predicates
  (list #'minibufferp
        (lambda () buffer-read-only)
        #'use-region-p)
  "Functions called with no arguments; if any returns non-nil, ecoin stays quiet."
  :type '(repeat function))

(defcustom ecoin-major-mode-alist
  '(("emacs-lisp" . "elisp") ("lisp-interaction" . "elisp") ("js" . "javascript")
    ("js2" . "javascript") ("rjsx" . "javascriptreact") ("tsx" . "typescriptreact")
    ("typescript-tsx" . "typescriptreact") ("c++" . "cpp") ("objc" . "objective-c")
    ("sh" . "shellscript") ("shell-script" . "shellscript") ("cperl" . "perl")
    ("enh-ruby" . "ruby") ("rustic" . "rust") ("ess-r" . "r") ("nxml" . "xml")
    ("text" . "plaintext") ("conf" . "ini") ("less-css" . "less")
    ("clojurescript" . "clojure") ("clojurec" . "clojure"))
  "Map from major mode name (without \"-mode\") to an LSP languageId."
  :type '(alist :key-type string :value-type string))

(defface ecoin-face
  '((t :inherit shadow))
  "Face for ghost text.")

;;;; Keymap (active only while ghost text is visible)

(defvar ecoin-completion-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "TAB") #'ecoin-accept)
    (define-key m (kbd "<tab>") #'ecoin-accept)
    (define-key m (kbd "C-TAB") #'ecoin-accept-word)
    (define-key m (kbd "C-<tab>") #'ecoin-accept-word)
    (define-key m (kbd "M-n") #'ecoin-next)
    (define-key m (kbd "M-p") #'ecoin-previous)
    m)
  "Keys that work while a suggestion is displayed.")

(defvar-local ecoin--overlay-active nil)
(defvar ecoin--emulation-alist `((ecoin--overlay-active . ,ecoin-completion-map)))
(add-to-list 'emulation-mode-map-alists 'ecoin--emulation-alist)

;;;; State

(defconst ecoin-version "0.1.0")
(defvar ecoin-mode)
(defvar ecoin--connection nil "The `jsonrpc-connection' to the server, or nil.")
(defvar ecoin--ready nil "Non-nil once the server has answered `initialize'.")
(defvar ecoin--last-connect-attempt nil
  "`float-time' of the last connection attempt, for `ecoin--connect-backoff'.")

(defconst ecoin--connect-backoff 5
  "Seconds before retrying a server that failed to come up.

Without this a server that dies on startup is respawned by every idle
tick -- one node process per keystroke -- because `ecoin--conn' no longer
blocks on the handshake and so no longer throttles itself.")
(defvar ecoin--request-counter 0 "Increases on every completion request.")
(defvar ecoin--status nil "Last status the server reported, as a plist.")
(defvar ecoin--status-message nil
  "Last status message shown, so the same one is not reported twice.")
(defvar ecoin--status-v2 nil
  "Non-nil once the server has sent `didChangeStatus/v2'.")
(defvar ecoin--workspace-folders nil "Folder URIs already announced to the server.")

(defvar-local ecoin--opened nil "Non-nil once didOpen was sent for this buffer.")
(defvar-local ecoin--version 0 "LSP document version.")
(defvar-local ecoin--synced-tick nil "`buffer-chars-modified-tick' at last sync.")
(defvar-local ecoin--last-tick nil "Tick seen by the previous post-command run.")
(defvar-local ecoin--timer nil)
(defvar-local ecoin--overlay nil)
(defvar-local ecoin--items nil "Vector of completion items from the last reply.")
(defvar-local ecoin--index 0)

;;;; Logging

(defun ecoin--log (fmt &rest args)
  "Append a line to the *ecoin-log* buffer."
  (with-current-buffer (get-buffer-create "*ecoin-log*")
    (goto-char (point-max))
    (insert (format-time-string "[%H:%M:%S] ") (apply #'format fmt args) "\n")))

;;;; Server connection

(defun ecoin--note-status (status)
  "Record STATUS and report it once when it is an error.
STATUS is a plist with `:kind', usually a `:message', and -- when it came
from `didChangeStatus/v2' -- a `:result' holding the sign-in state."
  (setq ecoin--status status)
  (let ((msg (plist-get status :message)))
    (cond
     ((not (equal (plist-get status :kind) "Error"))
      (setq ecoin--status-message nil))
     ;; The server announces one condition through both `didChangeStatus' and
     ;; `didChangeStatus/v2', so show any given message only once.
     ((equal msg ecoin--status-message))
     (t
      (setq ecoin--status-message msg)
      (message "ecoin: %s%s" msg
               (if (equal (plist-get (plist-get status :result) :status)
                          "NotSignedIn")
                   "  Run M-x ecoin-login"
                 ""))))))

(defun ecoin--handle-notification (_conn method params)
  (pcase method
    ('window/logMessage (ecoin--log "%s" (plist-get params :message)))
    ;; Both shapes describe the same condition and both are sent.  v2 is the
    ;; detailed one -- split by category, and its `auth' entry carries the
    ;; sign-in result -- so it wins, and the flat form is kept only for
    ;; servers old enough not to send v2 at all.
    ('didChangeStatus/v2
     (setq ecoin--status-v2 t)
     (when-let* ((auth (seq-find (lambda (s) (equal (plist-get s :category) "auth"))
                                 (append (plist-get params :statuses) nil))))
       (ecoin--note-status auth)))
    ('didChangeStatus
     (unless ecoin--status-v2 (ecoin--note-status params)))))

(defun ecoin--handle-request (_conn method params)
  (pcase method
    ('window/showMessageRequest
     (let* ((msg (plist-get params :message))
            (actions (append (plist-get params :actions) nil))
            (titles (mapcar (lambda (a) (plist-get a :title)) actions)))
       (if titles
           (let ((choice (completing-read (concat msg " ") titles nil t)))
             (cl-find choice actions :test #'equal
                      :key (lambda (a) (plist-get a :title))))
         (message "ecoin: %s" msg)
         nil)))
    ('window/showDocument
     (browse-url (plist-get params :uri))
     (list :success t))
    ('workspace/configuration
     (make-vector (length (plist-get params :items)) nil))
    (_ (jsonrpc-error "Method not supported: %s" method))))

(defun ecoin--on-shutdown (_conn)
  (setq ecoin--connection nil
        ecoin--ready nil
        ecoin--status nil
        ecoin--status-message nil
        ecoin--status-v2 nil
        ecoin--workspace-folders nil)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (setq ecoin--opened nil))))

(defun ecoin--purge-stale-cache ()
  "Delete `ecoin-purge-path-before-connect' if it exists and holds no files."
  (when-let* ((path ecoin-purge-path-before-connect)
              ((file-exists-p path)))
    ;; A symlink is unlinked rather than followed: the goal is only that the
    ;; path stop existing, and `delete-directory' on a link to a directory
    ;; would empty the target, which may live outside the config directory.
    (if (or (file-symlink-p path) (not (file-directory-p path)))
        (delete-file path)
      (unless (directory-files-recursively path "")
        (delete-directory path t)))))

(defun ecoin--connect ()
  "Start the server and begin the LSP handshake.  Return the connection.

The handshake is asynchronous deliberately.  It used to be a blocking
`jsonrpc-request', and `ecoin--conn' is reached from the idle timer that
fires while you type, so a slow or wedged server froze Emacs for the
whole 30-second timeout on the first keystroke.  Until `ecoin--ready'
flips, `ecoin--conn' reports no connection and callers stay quiet."
  (unless (executable-find (car ecoin-server-command))
    (error "ecoin: `%s' not found; install with: npm install -g @github/copilot-language-server"
           (car ecoin-server-command)))
  (ecoin--purge-stale-cache)
  (let ((conn (make-instance
               'jsonrpc-process-connection
               :name "ecoin"
               :process (lambda ()
                          (make-process :name "ecoin-server"
                                        :command ecoin-server-command
                                        :coding 'utf-8-emacs-unix
                                        :connection-type 'pipe
                                        :noquery t
                                        :stderr (get-buffer-create " *ecoin-stderr*")))
               :notification-dispatcher #'ecoin--handle-notification
               :request-dispatcher #'ecoin--handle-request
               :on-shutdown #'ecoin--on-shutdown)))
    (setq ecoin--connection conn
          ecoin--ready nil)
    (jsonrpc-async-request
     conn :initialize
     (list :processId (emacs-pid)
           :clientInfo (list :name "Emacs" :version emacs-version)
           :capabilities (list :workspace (list :workspaceFolders t))
           :initializationOptions
           (list :editorInfo (list :name "Emacs" :version emacs-version)
                 :editorPluginInfo (list :name "ecoin" :version ecoin-version))
           :rootUri nil
           :workspaceFolders [])
     :success-fn
     (lambda (_res)
       (jsonrpc-notify conn :initialized (make-hash-table))
       (setq ecoin--ready t)
       ;; The connection is made lazily on the first keystroke, so without
       ;; this there is nothing at all to distinguish "still starting up"
       ;; from "up, but with nothing to suggest here".  Sign-in trouble
       ;; arrives separately, through `ecoin--note-status'.
       (message "ecoin: server ready"))
     ;; No `checkStatus' here.  The server volunteers the sign-in state
     ;; through `didChangeStatus'/`didChangeStatus/v2' without being asked --
     ;; verified against a server that was never sent one -- so asking as well
     ;; only produced the same warning twice.
     :error-fn
     (lambda (e) (message "ecoin: initialize failed: %s" (plist-get e :message)))
     :timeout-fn
     (lambda () (message "ecoin: server did not answer initialize within 30s"))
     :timeout 30)
    conn))

(defun ecoin--conn ()
  "Return a ready connection, or nil if one is not usable yet.
Starts the server when it is not running.  Never blocks, so it is safe
to reach from `post-command-hook' and the idle timer."
  (if (and ecoin--connection (jsonrpc-running-p ecoin--connection))
      (and ecoin--ready ecoin--connection)
    (when (or (null ecoin--last-connect-attempt)
              (> (- (float-time) ecoin--last-connect-attempt)
                 ecoin--connect-backoff))
      (setq ecoin--last-connect-attempt (float-time))
      (ecoin--connect))
    nil))

(defun ecoin--conn-sync ()
  "Return a ready connection, waiting for the handshake to finish.
Blocks for up to 30 seconds, so only interactive commands may call this."
  ;; An explicit command is worth a connection attempt right now, so clear
  ;; the backoff rather than making the user wait it out.
  (unless (and ecoin--connection (jsonrpc-running-p ecoin--connection))
    (setq ecoin--last-connect-attempt nil))
  (or (ecoin--conn)
      (let ((deadline (+ (float-time) 30)))
        (while (and (not ecoin--ready) ecoin--connection (< (float-time) deadline))
          (accept-process-output nil 0.05))
        (unless (and ecoin--ready ecoin--connection)
          (error "ecoin: server did not finish initializing"))
        ecoin--connection)))

(defun ecoin--notify (method params)
  "Send METHOD with PARAMS, or do nothing while the server is not ready."
  (when-let* ((conn (ecoin--conn)))
    (jsonrpc-notify conn method params)))

;;;; Positions, URIs, language ids

(defun ecoin--utf16-length (string)
  "Length of STRING in UTF-16 code units."
  (+ (length string)
     (cl-count-if (lambda (c) (> c #xFFFF)) string)))

(defun ecoin--lsp-position (&optional pos)
  "Convert buffer position POS (default point) to an LSP position plist."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (or pos (point)))
      (list :line (1- (line-number-at-pos nil t))
            :character (ecoin--utf16-length
                        (buffer-substring-no-properties (line-beginning-position) (point)))))))

(defun ecoin--from-lsp-position (position)
  "Convert LSP POSITION plist to a buffer position."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (plist-get position :line))
      (let ((units (plist-get position :character)))
        (while (and (> units 0) (not (eolp)))
          (cl-decf units (if (> (char-after) #xFFFF) 2 1))
          (forward-char 1)))
      (point))))

(defun ecoin--uri ()
  (if buffer-file-name
      (concat "file://"
              (if (eq system-type 'windows-nt) "/" "")
              (url-hexify-string (file-local-name (expand-file-name buffer-file-name))
                                 url-path-allowed-chars))
    (concat "buffer://" (url-hexify-string (buffer-name)))))

(defun ecoin--language-id ()
  (let ((name (string-remove-suffix
               "-ts" (string-remove-suffix "-mode" (symbol-name major-mode)))))
    (or (cdr (assoc name ecoin-major-mode-alist)) name)))

(defun ecoin--indent-width ()
  "Best guess at the indentation width for this buffer."
  (or (cl-loop for var in '(python-indent-offset js-indent-level typescript-indent-level
                            typescript-ts-mode-indent-offset c-basic-offset rust-indent-offset
                            go-ts-mode-indent-offset ruby-indent-level css-indent-offset
                            sh-basic-offset lisp-indent-offset standard-indent)
               for val = (and (boundp var) (symbol-value var))
               when (integerp val) return val)
      tab-width))

;;;; Document sync

(defun ecoin--buffer-text ()
  (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max))))

(defun ecoin--announce-workspace ()
  "Tell the server about this buffer's project root, once."
  (when-let* ((proj (project-current))
              (root (project-root proj))
              (uri (concat "file://" (url-hexify-string (file-local-name (expand-file-name root))
                                                        url-path-allowed-chars))))
    (unless (member uri ecoin--workspace-folders)
      (push uri ecoin--workspace-folders)
      (ecoin--notify :workspace/didChangeWorkspaceFolders
                     (list :event (list :added (vector (list :uri uri :name (file-name-nondirectory
                                                                               (directory-file-name root))))
                                        :removed []))))))

(defun ecoin--sync ()
  "Make sure the server has the current buffer contents."
  (cond
   ((not ecoin--opened)
    (ecoin--announce-workspace)
    (setq ecoin--version 0
          ecoin--synced-tick (buffer-chars-modified-tick)
          ecoin--opened t)
    (ecoin--notify :textDocument/didOpen
                   (list :textDocument (list :uri (ecoin--uri)
                                             :languageId (ecoin--language-id)
                                             :version ecoin--version
                                             :text (ecoin--buffer-text)))))
   ((not (eq ecoin--synced-tick (buffer-chars-modified-tick)))
    (cl-incf ecoin--version)
    (setq ecoin--synced-tick (buffer-chars-modified-tick))
    (ecoin--notify :textDocument/didChange
                   (list :textDocument (list :uri (ecoin--uri) :version ecoin--version)
                         :contentChanges (vector (list :text (ecoin--buffer-text)))))))
  (ecoin--notify :textDocument/didFocus (list :textDocument (list :uri (ecoin--uri)))))

(defun ecoin--close ()
  (when (and ecoin--opened ecoin--connection (jsonrpc-running-p ecoin--connection))
    (ignore-errors
      (ecoin--notify :textDocument/didClose (list :textDocument (list :uri (ecoin--uri))))))
  (setq ecoin--opened nil))

;;;; Requesting completions

(defun ecoin--allowed-p ()
  (and ecoin-mode
       (<= (buffer-size) ecoin-max-chars)
       (not (cl-some #'funcall ecoin-disable-predicates))))

(defun ecoin--request (trigger-kind)
  "Ask the server for suggestions at point.
TRIGGER-KIND is 1 (manual) or 2 (automatic).

Does nothing while the handshake is still in flight -- `ecoin--sync' must
not run then, since it would mark the buffer opened without a `didOpen'
ever reaching the server.  The next keystroke retries."
  (condition-case err
      (when-let* ((conn (ecoin--conn)))
        (ecoin--sync)
        (let* ((buf (current-buffer))
               (pos (point))
               (tick (buffer-chars-modified-tick))
               (id (cl-incf ecoin--request-counter)))
          (jsonrpc-async-request
           conn :textDocument/inlineCompletion
           (list :textDocument (list :uri (ecoin--uri) :version ecoin--version)
                 :position (ecoin--lsp-position pos)
                 :context (list :triggerKind trigger-kind)
                 :formattingOptions (list :tabSize (ecoin--indent-width)
                                          :insertSpaces (if indent-tabs-mode :json-false t)))
           :success-fn
           (lambda (result)
             (when (and (= id ecoin--request-counter) (buffer-live-p buf))
               (with-current-buffer buf
                 (when (and (= (point) pos) (= tick (buffer-chars-modified-tick)))
                   (ecoin--handle-result result)))))
           :error-fn
           (lambda (err)
             (unless (eq (plist-get err :code) -32800) ; RequestCancelled
               (ecoin--log "completion error: %s" (plist-get err :message))))
           :timeout-fn #'ignore
           :timeout 20)))
    (error (message "ecoin: %s" (error-message-string err)))))

(defun ecoin--handle-result (result)
  (let ((items (plist-get result :items)))
    (if (or (null items) (zerop (length items)))
        (ecoin--clear-overlay)
      (setq ecoin--items items
            ecoin--index 0)
      (ecoin--show-item))))

(defun ecoin--show-item ()
  "Display item number `ecoin--index' from `ecoin--items' as ghost text."
  (let* ((item (aref ecoin--items ecoin--index))
         (text (plist-get item :insertText))
         (range (plist-get item :range))
         (start (ecoin--from-lsp-position (plist-get range :start)))
         (end (ecoin--from-lsp-position (plist-get range :end)))
         (prefix (buffer-substring-no-properties (min start (point)) (point))))
    (if (and (string-prefix-p prefix text) (> (length text) (length prefix)))
        (ecoin--display (substring text (length prefix))
                        (ecoin--utf16-length prefix)
                        (max end (point))
                        item)
      (ecoin--clear-overlay))))

;;;; Overlay

(defun ecoin--display (ghost prefix-len end item)
  "Show GHOST text at point.
PREFIX-LEN is how much of the item's insertText is already in the buffer,
END is where the text to be replaced ends, ITEM is the raw completion item."
  (ecoin--clear-overlay)
  (let* ((p (point))
         (str (propertize ghost 'face 'ecoin-face))
         (ov (if (eolp)
                 (make-overlay p p nil t t)
               (make-overlay p (1+ p) nil t t))))
    (put-text-property 0 1 'cursor t str)
    (if (eolp)
        (overlay-put ov 'after-string str)
      (overlay-put ov 'display (concat str (buffer-substring p (1+ p)))))
    (overlay-put ov 'ecoin-ghost ghost)
    (overlay-put ov 'ecoin-start p)
    (overlay-put ov 'ecoin-prefix-len prefix-len)
    (overlay-put ov 'ecoin-end (copy-marker end))
    (overlay-put ov 'ecoin-item item)
    (setq ecoin--overlay ov
          ecoin--overlay-active t)
    (ignore-errors
      (ecoin--notify :textDocument/didShowCompletion (list :item item)))))

(defun ecoin--clear-overlay ()
  (when ecoin--overlay
    (when-let* ((m (overlay-get ecoin--overlay 'ecoin-end))) (set-marker m nil))
    (delete-overlay ecoin--overlay))
  (setq ecoin--overlay nil
        ecoin--overlay-active nil))

(defun ecoin--visible-p ()
  (and ecoin--overlay (overlay-buffer ecoin--overlay)))

(defun ecoin--typed-into-ghost ()
  "If the last self-insert typed the ghost's next char, shrink the ghost. Return t."
  (when (and (ecoin--visible-p)
             (eq this-command 'self-insert-command)
             (= (point) (1+ (overlay-get ecoin--overlay 'ecoin-start))))
    (let ((ghost (overlay-get ecoin--overlay 'ecoin-ghost)))
      (when (and (> (length ghost) 1) (eq (char-before) (aref ghost 0)))
        (ecoin--display (substring ghost 1)
                        (1+ (overlay-get ecoin--overlay 'ecoin-prefix-len))
                        (marker-position (overlay-get ecoin--overlay 'ecoin-end))
                        (overlay-get ecoin--overlay 'ecoin-item))
        t))))

;;;; Accepting

(defun ecoin--accept (transform)
  "Insert TRANSFORM applied to the ghost text (nil means all of it)."
  (unless (ecoin--visible-p) (user-error "No suggestion to accept"))
  (let* ((ov ecoin--overlay)
         (ghost (overlay-get ov 'ecoin-ghost))
         (item (overlay-get ov 'ecoin-item))
         (prefix-len (overlay-get ov 'ecoin-prefix-len))
         (end (marker-position (overlay-get ov 'ecoin-end)))
         (text (if transform (funcall transform ghost) ghost))
         (partial (< (length text) (length ghost))))
    (ecoin--clear-overlay)
    (if partial
        (progn
          (insert text)
          (ecoin--notify :textDocument/didPartiallyAcceptCompletion
                         (list :item item
                               :acceptedLength (+ prefix-len (ecoin--utf16-length text))))
          (ecoin--display (substring ghost (length text))
                          (+ prefix-len (ecoin--utf16-length text))
                          (+ end (length text))
                          item))
      (delete-region (point) (max (point) end))
      (insert text)
      (when-let* ((cmd (plist-get item :command))
                  (conn (ecoin--conn)))
        (jsonrpc-async-request conn :workspace/executeCommand
                               (list :command (plist-get cmd :command)
                                     :arguments (plist-get cmd :arguments))
                               :success-fn #'ignore :error-fn #'ignore :timeout-fn #'ignore)))))

(defun ecoin-accept ()
  "Accept the whole suggestion."
  (interactive)
  (ecoin--accept nil))

(defun ecoin-accept-word ()
  "Accept the next word of the suggestion."
  (interactive)
  (ecoin--accept
   (lambda (ghost)
     (if (string-match "\\`[[:space:]\n]*\\(?:[[:word:]]+\\|[^[:space:][:word:]]\\)" ghost)
         (match-string 0 ghost)
       ghost))))

(defun ecoin-accept-line ()
  "Accept the next line of the suggestion."
  (interactive)
  (ecoin--accept
   (lambda (ghost)
     (if (string-match "\\`\n*[^\n]*" ghost) (match-string 0 ghost) ghost))))

(defun ecoin-dismiss ()
  "Hide the suggestion."
  (interactive)
  (ecoin--clear-overlay))

(defun ecoin-next ()
  "Show the next alternative suggestion."
  (interactive)
  (when (ecoin--visible-p)
    (setq ecoin--index (mod (1+ ecoin--index) (length ecoin--items)))
    (ecoin--show-item)))

(defun ecoin-previous ()
  "Show the previous alternative suggestion."
  (interactive)
  (when (ecoin--visible-p)
    (setq ecoin--index (mod (1- ecoin--index) (length ecoin--items)))
    (ecoin--show-item)))

(defun ecoin-complete ()
  "Request a suggestion at point right now."
  (interactive)
  (if (ecoin--conn)
      (ecoin--request 1)
    (message "ecoin: still connecting to the server; try again in a moment")))

(defconst ecoin--own-commands
  '(ecoin-accept ecoin-accept-word ecoin-accept-line ecoin-next ecoin-previous ecoin-complete))

;;;; Triggering

(defun ecoin--cancel-timer ()
  (when ecoin--timer (cancel-timer ecoin--timer) (setq ecoin--timer nil)))

(defun ecoin--idle (buf)
  (when (and (buffer-live-p buf) (eq buf (current-buffer)))
    (setq ecoin--timer nil)
    (when (ecoin--allowed-p) (ecoin--request 2))))

(defun ecoin--post-command ()
  (ecoin--cancel-timer)
  (let ((changed (not (eq ecoin--last-tick (buffer-chars-modified-tick)))))
    (setq ecoin--last-tick (buffer-chars-modified-tick))
    (cond
     ((memq this-command ecoin--own-commands) nil)
     ((ecoin--typed-into-ghost) nil)
     (t
      (ecoin--clear-overlay)
      (when (and changed ecoin-idle-delay (ecoin--allowed-p))
        (setq ecoin--timer (run-with-idle-timer ecoin-idle-delay nil
                                                #'ecoin--idle (current-buffer))))))))

;;;; Account commands

(defun ecoin-login ()
  "Sign in to GitHub Copilot with the device flow."
  (interactive)
  (let* ((conn (ecoin--conn-sync))
         (res (jsonrpc-request conn :signInInitiate (make-hash-table) :timeout 30)))
    (if (equal (plist-get res :status) "AlreadySignedIn")
        (message "ecoin: already signed in as %s" (plist-get res :user))
      (let ((code (plist-get res :userCode))
            (uri (plist-get res :verificationUri)))
        (kill-new code)
        (browse-url uri)
        (message "ecoin: enter code %s at %s (copied to kill ring)" code uri)
        (jsonrpc-async-request
         conn
         (if (plist-get res :command) :workspace/executeCommand :signInConfirm)
         (if-let* ((cmd (plist-get res :command)))
             (list :command (plist-get cmd :command) :arguments (plist-get cmd :arguments))
           (list :userCode code))
         :success-fn (lambda (r) (message "ecoin: signed in as %s (%s)"
                                          (plist-get r :user) (plist-get r :status)))
         :error-fn (lambda (e) (message "ecoin: sign-in failed: %s" (plist-get e :message)))
         :timeout-fn (lambda () (message "ecoin: sign-in timed out"))
         :timeout 600)))))

(defun ecoin-logout ()
  "Sign out of GitHub Copilot."
  (interactive)
  (jsonrpc-request (ecoin--conn-sync) :signOut (make-hash-table) :timeout 30)
  (message "ecoin: signed out"))

(defun ecoin-status ()
  "Show the sign-in status and last server status message."
  (interactive)
  (let ((res (jsonrpc-request (ecoin--conn-sync) :checkStatus (make-hash-table) :timeout 30)))
    (message "ecoin: %s%s%s"
             (plist-get res :status)
             (if-let* ((u (plist-get res :user))) (format " (%s)" u) "")
             (if-let* ((m (plist-get ecoin--status :message)))
                 (if (string-empty-p m) "" (format " — %s" m))
               ""))))

(defun ecoin-restart ()
  "Kill the server process; it restarts on the next request."
  (interactive)
  (when ecoin--connection (jsonrpc-shutdown ecoin--connection))
  (ecoin--on-shutdown nil)
  ;; Cleared here and not in `ecoin--on-shutdown', which also runs when the
  ;; server dies by itself -- resetting the backoff there would bring back the
  ;; respawn-per-keystroke loop it exists to prevent.
  (setq ecoin--last-connect-attempt nil)
  (message "ecoin: server stopped"))

;;;; Minor mode

;;;###autoload
(define-minor-mode ecoin-mode
  "Show GitHub Copilot inline suggestions as ghost text."
  :lighter " ecoin"
  (if ecoin-mode
      (progn
        (setq ecoin--last-tick (buffer-chars-modified-tick))
        (add-hook 'post-command-hook #'ecoin--post-command nil t)
        (add-hook 'kill-buffer-hook #'ecoin--close nil t))
    (ecoin--cancel-timer)
    (ecoin--clear-overlay)
    (remove-hook 'post-command-hook #'ecoin--post-command t)
    (remove-hook 'kill-buffer-hook #'ecoin--close t)
    (ecoin--close)))

(defun ecoin--turn-on ()
  (when (and (derived-mode-p 'prog-mode 'text-mode 'conf-mode)
             (not (minibufferp))
             (not buffer-read-only))
    (ecoin-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-ecoin-mode ecoin-mode ecoin--turn-on)

(provide 'ecoin)
;;; ecoin.el ends here
