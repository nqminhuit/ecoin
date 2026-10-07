;;; ecoin-copilot.el --- GitHub Copilot backend for ecoin  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, completion
;; URL: https://github.com/nqminhuit/ecoin

;;; Commentary:

;; The GitHub Copilot backend of ecoin.  It talks to the official
;; `copilot-language-server' over JSON-RPC using the built-in `jsonrpc'
;; library.  One server process is shared by all buffers.
;;
;; This file is loaded lazily by `ecoin' when `ecoin-backend' is `copilot'.
;; All LSP knowledge (UTF-16 positions, document sync, telemetry) lives here;
;; the core only sees `ecoin-item' structs.
;;
;; Setup:
;;   npm install -g @github/copilot-language-server
;;   M-x ecoin-login          ; first time only

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'jsonrpc)
(require 'url-util)
(require 'project)
(require 'ecoin)

;;;; Customization

(defcustom ecoin-server-command '("copilot-language-server" "--stdio")
  "Command (program and args) that starts the Copilot language server."
  :type '(repeat string)
  :group 'ecoin)

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
  :type '(choice (const :tag "Leave it alone" nil) directory)
  :group 'ecoin)

(defcustom ecoin-major-mode-alist
  '(("emacs-lisp" . "elisp") ("lisp-interaction" . "elisp") ("js" . "javascript")
    ("js2" . "javascript") ("rjsx" . "javascriptreact") ("tsx" . "typescriptreact")
    ("typescript-tsx" . "typescriptreact") ("c++" . "cpp") ("objc" . "objective-c")
    ("sh" . "shellscript") ("shell-script" . "shellscript") ("cperl" . "perl")
    ("enh-ruby" . "ruby") ("rustic" . "rust") ("ess-r" . "r") ("nxml" . "xml")
    ("text" . "plaintext") ("conf" . "ini") ("less-css" . "less")
    ("clojurescript" . "clojure") ("clojurec" . "clojure"))
  "Map from major mode name (without \"-mode\") to an LSP languageId."
  :type '(alist :key-type string :value-type string)
  :group 'ecoin)

;;;; State

(defvar ecoin-copilot--connection nil "The `jsonrpc-connection' to the server, or nil.")
(defvar ecoin-copilot--ready nil "Non-nil once the server has answered `initialize'.")
(defvar ecoin-copilot--last-connect-attempt nil
  "`float-time' of the last connection attempt.
Used by `ecoin-copilot--connect-backoff'.")

(defconst ecoin-copilot--connect-backoff 5
  "Seconds before retrying a server that failed to come up.

Without this a server that dies on startup is respawned by every idle
tick -- one node process per keystroke -- because `ecoin-copilot--conn'
no longer blocks on the handshake and so no longer throttles itself.")

(defvar ecoin-copilot--status nil "Last status the server reported, as a plist.")
(defvar ecoin-copilot--status-message nil
  "Last status message shown, so the same one is not reported twice.")
(defvar ecoin-copilot--status-v2 nil
  "Non-nil once the server has sent `didChangeStatus/v2'.")
(defvar ecoin-copilot--workspace-folders nil "Folder URIs already announced to the server.")

(defvar-local ecoin-copilot--opened nil "Non-nil once didOpen was sent for this buffer.")
(defvar-local ecoin-copilot--version 0 "LSP document version.")
(defvar-local ecoin-copilot--synced-tick nil "`buffer-chars-modified-tick' at last sync.")

;;;; Server connection

(defun ecoin-copilot--note-status (status)
  "Record STATUS and report it once when it is an error.
STATUS is a plist with `:kind', usually a `:message', and -- when it came
from `didChangeStatus/v2' -- a `:result' holding the sign-in state."
  (setq ecoin-copilot--status status)
  (let ((msg (plist-get status :message)))
    (cond
     ((not (equal (plist-get status :kind) "Error"))
      (setq ecoin-copilot--status-message nil))
     ;; The server announces one condition through both `didChangeStatus' and
     ;; `didChangeStatus/v2', so show any given message only once.
     ((equal msg ecoin-copilot--status-message))
     (t
      (setq ecoin-copilot--status-message msg)
      (message "ecoin: %s%s" msg
               (if (equal (plist-get (plist-get status :result) :status)
                          "NotSignedIn")
                   "  Run M-x ecoin-login"
                 ""))))))

(defun ecoin-copilot--handle-notification (_conn method params)
  (pcase method
    ('window/logMessage (ecoin--log "%s" (plist-get params :message)))
    ;; Both shapes describe the same condition and both are sent.  v2 is the
    ;; detailed one -- split by category, and its `auth' entry carries the
    ;; sign-in result -- so it wins, and the flat form is kept only for
    ;; servers old enough not to send v2 at all.
    ('didChangeStatus/v2
     (setq ecoin-copilot--status-v2 t)
     (when-let* ((auth (seq-find (lambda (s) (equal (plist-get s :category) "auth"))
                                 (append (plist-get params :statuses) nil))))
       (ecoin-copilot--note-status auth)))
    ('didChangeStatus
     (unless ecoin-copilot--status-v2 (ecoin-copilot--note-status params)))))

(defun ecoin-copilot--handle-request (_conn method params)
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

(defun ecoin-copilot--on-shutdown (_conn)
  (setq ecoin-copilot--connection nil
        ecoin-copilot--ready nil
        ecoin-copilot--status nil
        ecoin-copilot--status-message nil
        ecoin-copilot--status-v2 nil
        ecoin-copilot--workspace-folders nil)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (setq ecoin-copilot--opened nil))))

(defun ecoin-copilot--purge-stale-cache ()
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

(defun ecoin-copilot--connect ()
  "Start the server and begin the LSP handshake.  Return the connection.

The handshake is asynchronous deliberately.  It used to be a blocking
`jsonrpc-request', and `ecoin-copilot--conn' is reached from the idle timer that
fires while you type, so a slow or wedged server froze Emacs for the
whole 30-second timeout on the first keystroke.  Until `ecoin-copilot--ready'
flips, `ecoin-copilot--conn' reports no connection and callers stay quiet."
  (unless (executable-find (car ecoin-server-command))
    (error "ecoin: `%s' not found; install with: npm install -g @github/copilot-language-server"
           (car ecoin-server-command)))
  (ecoin-copilot--purge-stale-cache)
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
               :notification-dispatcher #'ecoin-copilot--handle-notification
               :request-dispatcher #'ecoin-copilot--handle-request
               :on-shutdown #'ecoin-copilot--on-shutdown)))
    (setq ecoin-copilot--connection conn
          ecoin-copilot--ready nil)
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
       (setq ecoin-copilot--ready t)
       ;; The connection is made lazily on the first keystroke, so without
       ;; this there is nothing at all to distinguish "still starting up"
       ;; from "up, but with nothing to suggest here".  Sign-in trouble
       ;; arrives separately, through `ecoin-copilot--note-status'.
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

(defun ecoin-copilot--conn ()
  "Return a ready connection, or nil if one is not usable yet.
Starts the server when it is not running.  Never blocks, so it is safe
to reach from `post-command-hook' and the idle timer."
  (if (and ecoin-copilot--connection (jsonrpc-running-p ecoin-copilot--connection))
      (and ecoin-copilot--ready ecoin-copilot--connection)
    (when (or (null ecoin-copilot--last-connect-attempt)
              (> (- (float-time) ecoin-copilot--last-connect-attempt)
                 ecoin-copilot--connect-backoff))
      (setq ecoin-copilot--last-connect-attempt (float-time))
      (ecoin-copilot--connect))
    nil))

(defun ecoin-copilot--conn-sync ()
  "Return a ready connection, waiting for the handshake to finish.
Blocks for up to 30 seconds, so only interactive commands may call this."
  ;; An explicit command is worth a connection attempt right now, so clear
  ;; the backoff rather than making the user wait it out.
  (unless (and ecoin-copilot--connection (jsonrpc-running-p ecoin-copilot--connection))
    (setq ecoin-copilot--last-connect-attempt nil))
  (or (ecoin-copilot--conn)
      (let ((deadline (+ (float-time) 30)))
        (while (and (not ecoin-copilot--ready) ecoin-copilot--connection (< (float-time) deadline))
          (accept-process-output nil 0.05))
        (unless (and ecoin-copilot--ready ecoin-copilot--connection)
          (error "ecoin: server did not finish initializing"))
        ecoin-copilot--connection)))

(defun ecoin-copilot--notify (method params)
  "Send METHOD with PARAMS, or do nothing while the server is not ready."
  (when-let* ((conn (ecoin-copilot--conn)))
    (jsonrpc-notify conn method params)))

;;;; Positions, URIs, language ids

(defun ecoin-copilot--utf16-length (string)
  "Length of STRING in UTF-16 code units."
  (+ (length string)
     (cl-count-if (lambda (c) (> c #xFFFF)) string)))

(defun ecoin-copilot--lsp-position (&optional pos)
  "Convert buffer position POS (default point) to an LSP position plist."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (or pos (point)))
      (list :line (1- (line-number-at-pos nil t))
            :character (ecoin-copilot--utf16-length
                        (buffer-substring-no-properties (line-beginning-position) (point)))))))

(defun ecoin-copilot--from-lsp-position (position)
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

(defun ecoin-copilot--uri ()
  (if buffer-file-name
      (concat "file://"
              (if (eq system-type 'windows-nt) "/" "")
              (url-hexify-string (file-local-name (expand-file-name buffer-file-name))
                                 url-path-allowed-chars))
    (concat "buffer://" (url-hexify-string (buffer-name)))))

(defun ecoin-copilot--language-id ()
  (let ((name (string-remove-suffix
               "-ts" (string-remove-suffix "-mode" (symbol-name major-mode)))))
    (or (cdr (assoc name ecoin-major-mode-alist)) name)))

(defun ecoin-copilot--indent-width ()
  "Best guess at the indentation width for this buffer."
  (or (cl-loop for var in '(python-indent-offset js-indent-level typescript-indent-level
                            typescript-ts-mode-indent-offset c-basic-offset rust-indent-offset
                            go-ts-mode-indent-offset ruby-indent-level css-indent-offset
                            sh-basic-offset lisp-indent-offset standard-indent)
               for val = (and (boundp var) (symbol-value var))
               when (integerp val) return val)
      tab-width))

;;;; Document sync

(defun ecoin-copilot--buffer-text ()
  (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max))))

(defun ecoin-copilot--announce-workspace ()
  "Tell the server about this buffer's project root, once."
  (when-let* ((proj (project-current))
              (root (project-root proj))
              (uri (concat "file://" (url-hexify-string (file-local-name (expand-file-name root))
                                                        url-path-allowed-chars))))
    (unless (member uri ecoin-copilot--workspace-folders)
      (push uri ecoin-copilot--workspace-folders)
      (ecoin-copilot--notify :workspace/didChangeWorkspaceFolders
                     (list :event (list :added (vector (list :uri uri :name (file-name-nondirectory
                                                                               (directory-file-name root))))
                                        :removed []))))))

(defun ecoin-copilot--sync ()
  "Make sure the server has the current buffer contents."
  (cond
   ((not ecoin-copilot--opened)
    (ecoin-copilot--announce-workspace)
    (setq ecoin-copilot--version 0
          ecoin-copilot--synced-tick (buffer-chars-modified-tick)
          ecoin-copilot--opened t)
    (ecoin-copilot--notify :textDocument/didOpen
                   (list :textDocument (list :uri (ecoin-copilot--uri)
                                             :languageId (ecoin-copilot--language-id)
                                             :version ecoin-copilot--version
                                             :text (ecoin-copilot--buffer-text)))))
   ((not (eq ecoin-copilot--synced-tick (buffer-chars-modified-tick)))
    (cl-incf ecoin-copilot--version)
    (setq ecoin-copilot--synced-tick (buffer-chars-modified-tick))
    (ecoin-copilot--notify :textDocument/didChange
                   (list :textDocument (list :uri (ecoin-copilot--uri) :version ecoin-copilot--version)
                         :contentChanges (vector (list :text (ecoin-copilot--buffer-text)))))))
  (ecoin-copilot--notify :textDocument/didFocus (list :textDocument (list :uri (ecoin-copilot--uri)))))

(defun ecoin-copilot--close ()
  (when (and ecoin-copilot--opened ecoin-copilot--connection (jsonrpc-running-p ecoin-copilot--connection))
    (ignore-errors
      (ecoin-copilot--notify :textDocument/didClose (list :textDocument (list :uri (ecoin-copilot--uri))))))
  (setq ecoin-copilot--opened nil))

;;;; Completion items

(defun ecoin-copilot--items (result)
  "Convert the inline-completion RESULT to a list of `ecoin-item'.
Must run in the request's buffer with point where it was requested.  An
LSP item replaces a range that usually starts before point (the text
already typed), so that prefix is stripped from the ghost text; items that
no longer extend the buffer are dropped."
  (let (out)
    (dolist (item (append (plist-get result :items) nil))
      (let* ((text (plist-get item :insertText))
             (range (plist-get item :range))
             (start (ecoin-copilot--from-lsp-position (plist-get range :start)))
             (end (ecoin-copilot--from-lsp-position (plist-get range :end)))
             (prefix (buffer-substring-no-properties (min start (point)) (point))))
        (when (and (string-prefix-p prefix text) (> (length text) (length prefix)))
          (push (ecoin-make-item :text (substring text (length prefix))
                                 :end (max end (point))
                                 :data (list :raw item :start (min start (point)))
                                 :backend 'copilot)
                out))))
    (nreverse out)))

;;;; Backend methods

  "Ask the server for suggestions for REQUEST at point in the current buffer.
CALLBACK receives the converted items.
  "Ask the server for suggestions at point in the current buffer.

Does nothing while the handshake is still in flight -- `ecoin-copilot--sync'
must not run then, since it would mark the buffer opened without a `didOpen'
ever reaching the server.  The next keystroke retries."
  (if-let* ((conn (ecoin-copilot--conn)))
      (let ((buf (ecoin-request-buffer request))
            (pos (ecoin-request-point request))
            (tick (ecoin-request-tick request)))
        (ecoin-copilot--sync)
        (jsonrpc-async-request
         conn :textDocument/inlineCompletion
         (list :textDocument (list :uri (ecoin-copilot--uri)
                                   :version ecoin-copilot--version)
               :position (ecoin-copilot--lsp-position pos)
               :context (list :triggerKind
                              (if (eq (ecoin-request-trigger request) 'manual) 1 2))
               :formattingOptions (list :tabSize (ecoin-copilot--indent-width)
                                        :insertSpaces (if indent-tabs-mode :json-false t)))
         :success-fn
         (lambda (result)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               ;; The conversion reads point, so it is only valid while the
               ;; buffer is as it was; the core re-checks on delivery.
               (when (and (= (point) pos) (= tick (buffer-chars-modified-tick)))
                 (funcall callback (ecoin-copilot--items result))))))
         :error-fn
         (lambda (err)
           (unless (eq (plist-get err :code) -32800) ; RequestCancelled
             (ecoin--log "completion error: %s" (plist-get err :message))))
         :timeout-fn #'ignore
         :timeout 20))
    (when (eq (ecoin-request-trigger request) 'manual)
      (message "ecoin: still connecting to the server; try again in a moment"))
    nil))

(cl-defmethod ecoin-backend-disable-buffer ((_backend (eql 'copilot)))
  "Tell the server the current buffer is closed."
  (ecoin-copilot--close))

(cl-defmethod ecoin-backend-shown ((_backend (eql 'copilot)) item)
  "Report to the server that ITEM was displayed."
  (ecoin-copilot--notify :textDocument/didShowCompletion
                         (list :item (plist-get (ecoin-item-data item) :raw))))

(cl-defmethod ecoin-backend-accepted ((_backend (eql 'copilot)) item _text partial)
  "Report an accept of ITEM to the server; point is just after the inserted text.
PARTIAL is non-nil when only part of the ghost text was taken."
  (let ((data (ecoin-item-data item)))
    (if partial
        (ecoin-copilot--notify
         :textDocument/didPartiallyAcceptCompletion
         (list :item (plist-get data :raw)
               :acceptedLength (ecoin-copilot--utf16-length
                                (buffer-substring-no-properties
                                 (plist-get data :start) (point)))))
      (when-let* ((cmd (plist-get (plist-get data :raw) :command))
                  (conn (ecoin-copilot--conn)))
        (jsonrpc-async-request conn :workspace/executeCommand
                               (list :command (plist-get cmd :command)
                                     :arguments (plist-get cmd :arguments))
                               :success-fn #'ignore :error-fn #'ignore :timeout-fn #'ignore)))))

(cl-defmethod ecoin-backend-status ((_backend (eql 'copilot)))
  "Return the sign-in status and last server status message."
  (let ((res (jsonrpc-request (ecoin-copilot--conn-sync) :checkStatus (make-hash-table)
                              :timeout 30)))
    (format "%s%s%s"
            (plist-get res :status)
            (if-let* ((u (plist-get res :user))) (format " (%s)" u) "")
            (if-let* ((m (plist-get ecoin-copilot--status :message)))
                (if (string-empty-p m) "" (format " — %s" m))
              ""))))

(cl-defmethod ecoin-backend-restart ((_backend (eql 'copilot)))
  "Kill the server process; it restarts on the next request."
  (when ecoin-copilot--connection (jsonrpc-shutdown ecoin-copilot--connection))
  (ecoin-copilot--on-shutdown nil)
  ;; Cleared here and not in `ecoin-copilot--on-shutdown', which also runs when
  ;; the server dies by itself -- resetting the backoff there would bring back
  ;; the respawn-per-keystroke loop it exists to prevent.
  (setq ecoin-copilot--last-connect-attempt nil)
  (message "ecoin: server stopped"))

;;;; Account commands

;;;###autoload
(defun ecoin-login ()
  "Sign in to GitHub Copilot with the device flow."
  (interactive)
  (let* ((conn (ecoin-copilot--conn-sync))
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

;;;###autoload
(defun ecoin-logout ()
  "Sign out of GitHub Copilot."
  (interactive)
  (jsonrpc-request (ecoin-copilot--conn-sync) :signOut (make-hash-table) :timeout 30)
  (message "ecoin: signed out"))

(provide 'ecoin-copilot)
;;; ecoin-copilot.el ends here
