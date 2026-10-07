;;; ecoin.el --- Inline completions (ghost text) with pluggable backends  -*- lexical-binding: t; -*-

;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, completion
;; URL: https://github.com/nqminhuit/ecoin

;;; Commentary:

;; ecoin shows inline suggestions (ghost text) and nothing else.  The core in
;; this file owns the minor modes, the idle trigger, the overlay and the
;; accept/cycle commands; where the suggestions come from is a backend,
;; selected with `ecoin-backend'.  Backends implement the `ecoin-backend-*'
;; generic functions below.
;;
;; Built-in backends are loaded on first use, so
;; nothing backend-specific is required just by loading this file:
;; `llama' (ecoin-llama.el, a llama.cpp server's /infill endpoint, the
;; default) and `copilot' (ecoin-copilot.el, the official
;; `copilot-language-server' over JSON-RPC).
;;
;; Setup (llama):
;;   llama-server --fim-qwen-1.5b-default
;;   (require 'ecoin)
;;   (add-hook 'prog-mode-hook #'ecoin-mode)
;; Setup (copilot):
;;   npm install -g @github/copilot-language-server
;;   (setq ecoin-backend 'copilot)
;;   M-x ecoin-login          ; first time only
;;
;; While a suggestion is visible: TAB accepts it, C-TAB accepts one word,
;; M-n / M-p cycle alternatives, any other key dismisses it.
;;
;; `M-x ecoin-stats' shows in-memory statistics (nothing is written to disk).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;;; Customization

(defgroup ecoin nil
  "Inline completions."
  :group 'completion
  :prefix "ecoin-")

(defcustom ecoin-backend 'llama
  "Backend that supplies suggestions.
A symbol on which the `ecoin-backend-*' generic functions dispatch."
  :type '(choice (const :tag "llama.cpp server (/infill)" llama)
                 (const :tag "GitHub Copilot" copilot)))

(defcustom ecoin-fallback-backend nil
  "Backend that serves requests while `ecoin-backend' is unavailable, or nil.
Setting this is your consent to send code to that backend, for example
GitHub for `copilot', whenever the primary backend reports itself
unavailable.  There is no per-request prompt.  Buffers excluded by
`ecoin-exclude-file-regexps' and `ecoin-exclude-functions' are never sent.
A value equal to `ecoin-backend' is ignored."
  :type '(choice (const :tag "No fallback" nil)
                 (const :tag "GitHub Copilot" copilot)
                 (const :tag "llama.cpp server (/infill)" llama)))

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

(defcustom ecoin-enable-predicates '(ecoin-insert-state-p)
  "Functions called with no arguments; ecoin acts only if all return non-nil.
Entries that are not functions count as satisfied, so a predicate from a
package that is not installed does no harm."
  :type '(repeat function))

(defcustom ecoin-overlay-priority 100
  "Priority of the ghost text overlay, relative to other overlays at point."
  :type 'integer)

(defcustom ecoin-exclude-file-regexps
  '("/\\.env[^/]*\\'"
    "/\\.authinfo[^/]*\\'"
    "/\\.netrc\\'"
    "\\.\\(?:gpg\\|age\\|pem\\|key\\)\\'"
    "/id_\\(?:rsa\\|ed25519\\)[^/]*\\'"
    "/\\.\\(?:ssh\\|aws\\|gnupg\\|password-store\\)/"
    "/secrets\\.\\(?:ya?ml\\|json\\|env\\)\\'")
  "Buffers visiting a file that matches one of these regexps get no suggestions.
Matched against the buffer's expanded file name, whatever the backend."
  :type '(repeat regexp))

(defcustom ecoin-exclude-functions nil
  "Abnormal hook: functions called with no arguments in the buffer.
If any returns non-nil the buffer is excluded, like a match of
`ecoin-exclude-file-regexps'."
  :type 'hook)

(defcustom ecoin-log-content nil
  "Non-nil allows buffer text in *ecoin-log*; by default it holds no content."
  :type 'boolean)

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

(defvar-local ecoin--overlay-active nil
  "Non-nil while ghost text is displayed; kept for configs that test it.")

(defconst ecoin--keymap-priority 101
  "Priority of the keymap overlay; above the ghost overlay's default.")

;;;; Backend protocol

(cl-defstruct (ecoin-request (:constructor ecoin--make-request))
  "A request for suggestions at one point in one buffer."
  id buffer point tick
  trigger                               ; auto | manual | move
  time)                                 ; `float-time' when it was made

(cl-defstruct (ecoin-item (:constructor ecoin-make-item))
  "One suggestion, already normalized for display."
  text                                  ; ghost text to insert at point
  (end nil)                             ; buffer pos >= point to replace up to; nil = point
  data                                  ; opaque backend payload
  backend)                              ; backend that produced it

(cl-defgeneric ecoin-backend-request (backend _request _callback)
  "Ask BACKEND for suggestions for REQUEST, an `ecoin-request'.
Called with REQUEST's buffer current.  Must not block.  CALLBACK takes a
list of `ecoin-item' (possibly empty) and may be called before this
returns, e.g. on a cache hit.  Return a handle for `ecoin-backend-cancel'."
  (error "ecoin: no backend `%s'" backend))

(cl-defgeneric ecoin-backend-cancel (_backend _handle)
  "Cancel the request identified by HANDLE.  The default does nothing."
  nil)

(cl-defgeneric ecoin-backend-enable-buffer (_backend)
  "Called in a buffer when `ecoin-mode' turns on, if BACKEND is loaded."
  nil)

(cl-defgeneric ecoin-backend-disable-buffer (_backend)
  "Called in a buffer when `ecoin-mode' turns off or the buffer is killed."
  nil)

(cl-defgeneric ecoin-backend-shown (_backend _item)
  "Called when ITEM, produced by BACKEND, is displayed."
  nil)

(cl-defgeneric ecoin-backend-accepted (_backend _item _text _partial)
  "Called after TEXT of ITEM was inserted; PARTIAL if ghost text remains."
  nil)

(cl-defgeneric ecoin-backend-status (backend)
  "Return a status string for BACKEND, shown by `ecoin-status'."
  (format "backend `%s' reports no status" backend))

(cl-defgeneric ecoin-backend-capabilities (_backend)
  "Return a plist of BACKEND capabilities, e.g. (:trigger-on-move t)."
  nil)

(cl-defgeneric ecoin-backend-mode-line (_backend)
  "Return a short string appended to the mode-line lighter, or nil."
  nil)

(cl-defgeneric ecoin-backend-available-p (_backend)
  "Return non-nil if BACKEND can serve requests right now."
  t)

(cl-defgeneric ecoin-backend-unavailable-reason (_backend)
  "Return a short phrase saying why BACKEND is unavailable, or nil."
  nil)

(cl-defgeneric ecoin-backend-restart (_backend)
  "Restart BACKEND's server, if it has one.  The default does nothing."
  nil)

;; Called from `ecoin-stats'; see `ecoin--stats-report'.
(cl-defgeneric ecoin-backend-stats (_backend)
  "Return extra statistics of BACKEND as a list of (LABEL . TEXT) strings, or nil."
  nil)

(cl-defgeneric ecoin-backend-stats-reset (_backend)
  "Forget the statistics BACKEND keeps for `ecoin-backend-stats'."
  nil)

(defconst ecoin--backend-features '((llama . ecoin-llama)
                                    (copilot . ecoin-copilot))
  "Feature that provides each built-in backend, loaded on first use.")

(defun ecoin--ensure-backend (backend)
  "Load the file that implements BACKEND, if it is a built-in one."
  (when-let* ((feature (alist-get backend ecoin--backend-features)))
    (require feature)))

(defconst ecoin--backend-labels '((llama . "local llama server") (copilot . "Copilot"))
  "Names of the built-in backends in messages.")

(defconst ecoin--backend-markers '((copilot . "cp"))
  "Short names of backends for the mode-line while they serve as fallback.")

(defun ecoin--backend-label (backend)
  "Name of BACKEND for messages."
  (or (alist-get backend ecoin--backend-labels) (symbol-name backend)))

;; Declared here so they work after `(require 'ecoin)' without a package
;; manager generating autoloads.
(autoload 'ecoin-login "ecoin-copilot" "Sign in to GitHub Copilot." t)
(autoload 'ecoin-logout "ecoin-copilot" "Sign out of GitHub Copilot." t)

;;;; State

(defconst ecoin-version "0.2.0")
(defvar ecoin-mode)
(defvar ecoin--request-counter 0 "Increases on every completion request.")

(defvar-local ecoin--last-tick nil "Tick seen by the previous post-command run.")
(defvar-local ecoin--timer nil)
(defvar-local ecoin--overlay nil)
(defvar-local ecoin--keymap-overlay nil "Overlay at point carrying `ecoin-completion-map'.")
(defvar-local ecoin--last-point nil "Point seen by the previous post-command run.")
(defvar-local ecoin--pending nil "(BACKEND . HANDLE) of the request in flight.")
(defvar-local ecoin--items nil "Vector of `ecoin-item' from the last reply.")
(defvar-local ecoin--index 0)
(defvar-local ecoin--served-by nil
  "Backends that have served this buffer, so each can be told when it closes.")
(defvar-local ecoin--stat-session nil
  "(BACKEND . ACCEPTED) while a ghost shown in this buffer is unresolved.")
(defvar ecoin--fallback-state nil
  "(PRIMARY . FALLBACK) while requests are routed to the fallback, else nil.")

;;;; Logging

(defun ecoin--log (fmt &rest args)
  "Append a line to the *ecoin-log* buffer."
  (with-current-buffer (get-buffer-create "*ecoin-log*")
    (goto-char (point-max))
    (insert (format-time-string "[%H:%M:%S] ") (apply #'format fmt args) "\n")))

(defun ecoin--hook (fn &rest args)
  "Call backend hook FN with ARGS; log instead of signalling.
Telemetry hooks must never break editing."
  (condition-case err
      (apply fn args)
    ;; The message of an error may quote buffer text, e.g. `args-out-of-range'.
    (error (ecoin--log "%s: %s" fn (if ecoin-log-content
                                       (error-message-string err)
                                     (car err))))))

;;;; Statistics
;;
;; In memory only.  Events go into a fixed ring of `ecoin--stats-size' slots,
;; so recording is O(1) and the memory use is bounded.

(defconst ecoin--stats-size 500 "Number of events kept.")

(cl-defstruct (ecoin--ring (:constructor ecoin--make-ring (size &aux (vec (make-vector size nil)))))
  "Fixed-size ring of the newest SIZE items."
  size vec (next 0) (count 0))

(defun ecoin--ring-push (ring item)
  "Add ITEM to RING in O(1), dropping the oldest when full."
  (let ((size (ecoin--ring-size ring)))
    (aset (ecoin--ring-vec ring) (ecoin--ring-next ring) item)
    (setf (ecoin--ring-next ring) (mod (1+ (ecoin--ring-next ring)) size)
          (ecoin--ring-count ring) (min size (1+ (ecoin--ring-count ring))))))

(defun ecoin--ring-items (ring)
  "The items of RING, oldest first."
  (let* ((size (ecoin--ring-size ring))
         (count (ecoin--ring-count ring))
         (start (mod (- (ecoin--ring-next ring) count) size)))
    (cl-loop for i below count
             collect (aref (ecoin--ring-vec ring) (mod (+ start i) size)))))

(defun ecoin--ring-clear (ring)
  "Empty RING."
  (fillarray (ecoin--ring-vec ring) nil)
  (setf (ecoin--ring-next ring) 0 (ecoin--ring-count ring) 0))

(defvar ecoin--stats (ecoin--make-ring ecoin--stats-size)
  "Ring of (KIND BACKEND VALUE) events.
KIND is `latency' (VALUE seconds), `shown', `dismissed' or `accepted' (VALUE
is `full', `word', `line' or `typed' for the first accept of a ghost).")

(defun ecoin--stat (kind backend &optional value)
  "Record an event; never signal, since this runs inside command hooks."
  (condition-case nil
      (ecoin--ring-push ecoin--stats (list kind backend value))
    (error nil)))

(defun ecoin--stat-accepted (kind)
  "Note an accept of KIND in the open ghost session; only the first one counts."
  (condition-case nil
      (when-let* ((session ecoin--stat-session))
        (unless (cdr session)
          (setcdr session t)
          (ecoin--stat 'accepted (car session) kind)))
    (error nil)))

(defun ecoin--stat-end-session (dismissed)
  "Close the open ghost session; with DISMISSED, count it unless it was accepted."
  (when-let* ((session ecoin--stat-session))
    (setq ecoin--stat-session nil)
    (when (and dismissed (not (cdr session)))
      (ecoin--stat 'dismissed (car session)))))

(defun ecoin--percentile (sorted p)
  "The P-th percentile (0..1) of the non-empty ascending list SORTED, nearest rank."
  (nth (max 0 (1- (ceiling (* p (length sorted))))) sorted))

(defun ecoin--median (numbers)
  "Median of the non-nil NUMBERS, or nil if there are none."
  (when-let* ((sorted (sort (delq nil (copy-sequence numbers)) #'<)))
    (ecoin--percentile sorted 0.5)))

(defun ecoin--stats-report ()
  "The statistics as text."
  (let ((by-backend nil))
    (dolist (event (ecoin--ring-items ecoin--stats))
      (push event (alist-get (cadr event) by-backend)))
    (if (null by-backend)
        "No ecoin events recorded yet.\n"
      (with-temp-buffer
        (insert (format "ecoin statistics, last %d events, in memory only\n"
                        (ecoin--ring-count ecoin--stats)))
        (dolist (entry (nreverse by-backend))
          (insert "\n" (ecoin--stats-backend-report (car entry) (cdr entry))))
        (buffer-string)))))

(defun ecoin--stats-backend-report (backend events)
  "Text for BACKEND from its EVENTS (newest first)."
  (let* ((of (lambda (kind) (seq-filter (lambda (e) (eq (car e) kind)) events)))
         (latency (sort (mapcar #'caddr (funcall of 'latency)) #'<))
         (shown (length (funcall of 'shown)))
         (accepted (funcall of 'accepted))
         (kinds (mapcar #'caddr accepted))
         (n (length accepted))
         (partial (+ (cl-count 'word kinds) (cl-count 'line kinds)))
         (extra (condition-case nil (ecoin-backend-stats backend) (error nil))))
    (concat
     (format "%s\n" backend)
     (if latency
         (format "  latency        p50 %d ms, p95 %d ms (%d ghosts, trigger to shown)\n"
                 (round (* 1000 (ecoin--percentile latency 0.5)))
                 (round (* 1000 (ecoin--percentile latency 0.95)))
                 (length latency))
       "  latency        no data\n")
     (format "  shown          %d\n" shown)
     (format "  accepted       %d%s (full %d, partial %d: word %d, line %d)\n"
             n (if (> shown 0) (format " = %d%%" (round (* 100.0 n) shown)) "")
             (- n partial) partial (cl-count 'word kinds) (cl-count 'line kinds))
     (format "  dismissed      %d\n" (length (funcall of 'dismissed)))
     (mapconcat (lambda (row) (format "  %-14s %s\n" (car row) (cdr row))) extra ""))))

;;;###autoload
(defun ecoin-stats ()
  "Show statistics about recent suggestions in a read-only buffer.
Kept in memory only, for the last 500 events; see `ecoin-stats-reset'."
  (interactive)
  (let ((text (ecoin--stats-report)))
    (with-current-buffer (get-buffer-create "*ecoin-stats*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (special-mode)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun ecoin-stats-reset ()
  "Forget all recorded statistics, including those of the backends."
  (interactive)
  (ecoin--ring-clear ecoin--stats)
  (dolist (backend (mapcar #'car ecoin--backend-features))
    (when (featurep (alist-get backend ecoin--backend-features))
      (ecoin--hook #'ecoin-backend-stats-reset backend)))
  (message "ecoin: statistics cleared"))

;;;; Requesting completions

;;;; Gates

(defvar corfu--frame)
(defvar company-candidates)
(defvar multiple-cursors-mode)
(defvar so-long-minor-mode)
(declare-function evil-insert-state-p "evil-states")
(declare-function evil-emacs-state-p "evil-states")
(declare-function evil-mc-has-cursors-p "evil-mc-cursor-state")

(defun ecoin-insert-state-p ()
  "Non-nil if evil is not active in this buffer, or is in insert or emacs state."
  (or (not (bound-and-true-p evil-local-mode))
      (not (fboundp 'evil-insert-state-p))
      (evil-insert-state-p)
      (and (fboundp 'evil-emacs-state-p) (evil-emacs-state-p))))

(defun ecoin--excluded-p ()
  "Non-nil if this buffer must never be sent anywhere."
  (condition-case nil
      (or (and buffer-file-name
               (let ((file (expand-file-name buffer-file-name))
                     (case-fold-search nil))
                 (cl-some (lambda (re) (string-match-p re file))
                          ecoin-exclude-file-regexps)))
          (run-hook-with-args-until-success 'ecoin-exclude-functions))
    ;; Fail closed: this runs from a command hook, which must not break.
    (error t)))

(defun ecoin--popup-p ()
  "Non-nil while a completion popup is showing."
  (or (bound-and-true-p completion-in-region-mode)
      (and (boundp 'corfu--frame)
           (frame-live-p corfu--frame)
           (frame-visible-p corfu--frame))
      (and (boundp 'company-candidates) company-candidates)))

(defun ecoin--quiet-p ()
  "Non-nil in situations where ghost text would be wrong or useless."
  (or (ecoin--popup-p)
      (bound-and-true-p multiple-cursors-mode)
      (and (fboundp 'evil-mc-has-cursors-p) (evil-mc-has-cursors-p))
      executing-kbd-macro
      (bound-and-true-p so-long-minor-mode)
      (eq major-mode 'so-long-mode)
      (invisible-p (point))))

(defun ecoin--may-show-p ()
  "Non-nil if ghost text may be displayed here, whatever the trigger."
  (and (not (ecoin--excluded-p))
       (not (ecoin--quiet-p))
       (cl-every (lambda (fn) (or (not (functionp fn)) (funcall fn)))
                 ecoin-enable-predicates)))

(defun ecoin--allowed-p ()
  "Non-nil if an automatic request may be made now."
  (and ecoin-mode
       (<= (buffer-size) ecoin-max-chars)
       (ecoin--may-show-p)
       (not (cl-some #'funcall ecoin-disable-predicates))))

(defun ecoin--cancel-pending ()
  "Cancel the request in flight, if any."
  (when-let* ((pending ecoin--pending))
    (setq ecoin--pending nil)
    (ecoin--hook #'ecoin-backend-cancel (car pending) (cdr pending))))

(defun ecoin--fallback-in-use ()
  "The fallback backend if requests are routed to it now, else nil."
  (and ecoin--fallback-state
       (eq (car ecoin--fallback-state) ecoin-backend)
       (eq (cdr ecoin--fallback-state) ecoin-fallback-backend)
       (cdr ecoin--fallback-state)))

(defun ecoin--route (primary)
  "Return the backend to ask now: PRIMARY, or the fallback while it is unavailable.
Announces each switch once.  Sends nothing."
  (ecoin--ensure-backend primary)
  (let ((fallback ecoin-fallback-backend))
    (if (and fallback
             (not (eq fallback primary))
             (not (ecoin-backend-available-p primary)))
        (progn
          (ecoin--ensure-backend fallback)
          (unless (ecoin--fallback-in-use)
            (setq ecoin--fallback-state (cons primary fallback))
            (let ((reason (ecoin-backend-unavailable-reason primary)))
              (message "ecoin: %s unavailable%s; using %s"
                       (ecoin--backend-label primary)
                       (if reason (format " (%s)" reason) "")
                       (ecoin--backend-label fallback)))
            (force-mode-line-update t))
          fallback)
      primary)))

(defun ecoin--note-primary-served (primary)
  "PRIMARY just answered: if we were on the fallback, say we are back."
  (when (and ecoin--fallback-state (eq (car ecoin--fallback-state) primary))
    (setq ecoin--fallback-state nil)
    (message "ecoin: %s is back" (ecoin--backend-label primary))
    (force-mode-line-update t)))

(defun ecoin--trigger-on-move-p ()
  "Non-nil if the backend now in use asks for requests on cursor moves."
  (plist-get (ecoin-backend-capabilities (or (ecoin--fallback-in-use)
                                             ecoin-backend))
             :trigger-on-move))

(defun ecoin--after-full-accept ()
  "Ask for the next suggestion at once after a whole one was accepted.
Only for backends that follow the cursor; for llama it is usually a cache
hit on the prefetched continuation."
  (when (and (ecoin--trigger-on-move-p) (ecoin--allowed-p))
    (ecoin--request 'auto)))

(defun ecoin--request (trigger)
  "Ask the backend for suggestions at point.
TRIGGER is `auto' or `manual'."
  (condition-case err
      (unless (ecoin--excluded-p)
        (let* ((primary ecoin-backend)
               (backend (ecoin--route primary))
               (req (ecoin--make-request :id (cl-incf ecoin--request-counter)
                                         :buffer (current-buffer)
                                         :point (point)
                                         :tick (buffer-chars-modified-tick)
                                         :trigger trigger
                                         :time (float-time))))
          (ecoin--ensure-backend backend)
          (ecoin--cancel-pending)
          (cl-pushnew backend ecoin--served-by)
          ;; The callback may run before the backend returns its handle, in
          ;; which case there is nothing left to cancel.
          (let* ((done nil)
                 (handle (ecoin-backend-request
                          backend req (lambda (items)
                                        (setq done t)
                                        (when (eq backend primary)
                                          (ecoin--note-primary-served primary))
                                        (ecoin--deliver backend req items)))))
            (setq ecoin--pending (and handle (not done) (cons backend handle))))))
    (error (message "ecoin: %s" (error-message-string err)))))

(defun ecoin--deliver (backend req items)
  "Show ITEMS from BACKEND for REQ, unless the world moved on meanwhile."
  (let ((buf (ecoin-request-buffer req)))
    (when (and (= (ecoin-request-id req) ecoin--request-counter)
               (buffer-live-p buf))
      (with-current-buffer buf
        (when (and ecoin-mode
                   (ecoin--may-show-p)
                   (= (point) (ecoin-request-point req))
                   (= (ecoin-request-tick req) (buffer-chars-modified-tick)))
          (setq ecoin--pending nil)
          (let ((shown (seq-filter (lambda (item)
                                     (unless (ecoin-item-backend item)
                                       (setf (ecoin-item-backend item) backend))
                                     (> (length (ecoin-item-text item)) 0))
                                   (mapcar #'ecoin--dedent-item items))))
            (ecoin--handle-items shown)
            (when (and shown (ecoin-request-time req))
              (ecoin--stat 'latency (ecoin-item-backend (car shown))
                           (- (float-time) (ecoin-request-time req))))))))))

(defun ecoin--handle-items (items)
  (if (null items)
      (ecoin--clear-overlay)
    (setq ecoin--items (vconcat items)
          ecoin--index 0)
    (ecoin--show-item)))

(defun ecoin--show-item ()
  "Display item number `ecoin--index' from `ecoin--items' as ghost text."
  (let ((item (aref ecoin--items ecoin--index)))
    (ecoin--display item)
    ;; Cycling with M-n keeps one session: it ends with an accept or a dismiss.
    (unless ecoin--stat-session
      (setq ecoin--stat-session (cons (ecoin-item-backend item) nil)))
    (ecoin--stat 'shown (ecoin-item-backend item))
    (ecoin--hook #'ecoin-backend-shown (ecoin-item-backend item) item)))

;;;; Overlay

(defun ecoin--display (item)
  "Show the text of ITEM as ghost text at point."
  (ecoin--delete-overlay)
  (let* ((p (point))
         (eol (eolp))
         (ghost (ecoin-item-text item))
         ;; `cursor' does not work on a newline, so a leading one gets a
         ;; visible space in front of it to carry the property.
         (str (propertize (if (string-prefix-p "\n" ghost) (concat " " ghost) ghost)
                          'face 'ecoin-face))
         (ov (make-overlay p p nil t t))
         (kov (make-overlay p (min (1+ p) (point-max)) nil nil t)))
    ;; Non-integer `cursor' is ignored when point is visible, i.e. mid-line.
    (put-text-property 0 1 'cursor (if eol t 1) str)
    (overlay-put ov 'after-string str)
    (overlay-put ov 'window (selected-window))
    (overlay-put ov 'priority ecoin-overlay-priority)
    (overlay-put ov 'ecoin-start p)
    (overlay-put ov 'ecoin-end (copy-marker (max (or (ecoin-item-end item) p) p)))
    (overlay-put ov 'ecoin-item item)
    ;; A `keymap' property at point outranks evil's and corfu's maps whatever
    ;; their load order.
    (overlay-put kov 'keymap ecoin-completion-map)
    (overlay-put kov 'priority ecoin--keymap-priority)
    (setq ecoin--overlay ov
          ecoin--keymap-overlay kov
          ecoin--overlay-active t)))

(defun ecoin--overlay-ghost ()
  "The ghost text currently displayed."
  (ecoin-item-text (overlay-get ecoin--overlay 'ecoin-item)))

(defun ecoin--clear-overlay ()
  "Hide the ghost; a ghost that was never accepted counts as dismissed."
  (when ecoin--overlay
    (ecoin--stat-end-session t))
  (ecoin--delete-overlay))

(defun ecoin--delete-overlay ()
  "Remove the ghost's overlays without touching the statistics."
  (when ecoin--overlay
    (when-let* ((m (overlay-get ecoin--overlay 'ecoin-end))) (set-marker m nil))
    (delete-overlay ecoin--overlay))
  (when ecoin--keymap-overlay (delete-overlay ecoin--keymap-overlay))
  (setq ecoin--overlay nil
        ecoin--keymap-overlay nil
        ecoin--overlay-active nil))

(defun ecoin--visible-p ()
  (and ecoin--overlay (overlay-buffer ecoin--overlay)))

(defun ecoin--remainder (item text-offset end)
  "Copy of ITEM without its first TEXT-OFFSET chars, replacing up to END."
  (let ((rest (copy-ecoin-item item)))
    (setf (ecoin-item-text rest) (substring (ecoin-item-text item) text-offset)
          (ecoin-item-end rest) end)
    rest))

(defun ecoin--typed-into-ghost ()
  "Follow a self-insert that typed the ghost's next char.  Return t if it did.
Typing the last char accepts the item."
  (when (and (ecoin--visible-p)
             (eq this-command 'self-insert-command)
             (= (point) (1+ (overlay-get ecoin--overlay 'ecoin-start))))
    (let* ((ov ecoin--overlay)
           (item (overlay-get ov 'ecoin-item))
           (ghost (ecoin-item-text item))
           (end (marker-position (overlay-get ov 'ecoin-end))))
      (when (eq (char-before) (aref ghost 0))
        (if (> (length ghost) 1)
            (ecoin--display (ecoin--remainder item 1 end))
          (ecoin--delete-overlay)
          (ecoin--stat-accepted 'typed)
          (ecoin--stat-end-session nil)
          (delete-region (point) (max (point) end))
          (ecoin--hook #'ecoin-backend-accepted (ecoin-item-backend item) item ghost nil)
          (ecoin--after-full-accept))
        t))))

;;;; Accepting

(defun ecoin--dedent-item (item)
  "Return ITEM, or a copy without the indentation already before point.
Done on delivery so that the ghost shows exactly what accepting inserts."
  (let ((text (ecoin-item-text item)))
    (if (equal text (ecoin--dedent text))
        item
      (let ((copy (copy-ecoin-item item)))
        (setf (ecoin-item-text copy) (ecoin--dedent text))
        copy))))

(defun ecoin--dedent (text)
  "Drop from TEXT the indentation already before point, if point is in indentation.
Models often repeat the indentation of the current line at the start of
their suggestion."
  (let ((existing (save-excursion
                    (let ((here (point)))
                      (skip-chars-backward " \t")
                      (and (bolp) (- here (point)))))))
    (if (and existing (string-match "\\`[ \t]+" text))
        (substring text (min (match-end 0) existing))
      text)))

(defun ecoin--accept (transform &optional kind)
  "Insert TRANSFORM applied to the ghost text (nil means all of it).
KIND is `full', `word' or `line', for the statistics."
  (unless (ecoin--visible-p) (user-error "No suggestion to accept"))
  (let* ((ov ecoin--overlay)
         (item (overlay-get ov 'ecoin-item))
         (ghost (ecoin-item-text item))
         (end (marker-position (overlay-get ov 'ecoin-end)))
         (text (if transform (funcall transform ghost) ghost))
         (partial (< (length text) (length ghost))))
    (ecoin--delete-overlay)
    (ecoin--stat-accepted (if partial kind 'full))
    (unless partial (ecoin--stat-end-session nil))
    (if partial
        (progn
          (insert text)
          (ecoin--hook #'ecoin-backend-accepted (ecoin-item-backend item) item text t)
          (ecoin--display (ecoin--remainder item (length text) (+ end (length text)))))
      (delete-region (point) (max (point) end))
      (insert text)
      (ecoin--hook #'ecoin-backend-accepted (ecoin-item-backend item) item text nil)
      (ecoin--after-full-accept))))

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
       ghost))
   'word))

(defun ecoin-accept-line ()
  "Accept the next line of the suggestion."
  (interactive)
  (ecoin--accept
   (lambda (ghost)
     (if (string-match "\\`\n*[^\n]*" ghost) (match-string 0 ghost) ghost))
   'line))

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
  (if (ecoin--excluded-p)
      (message "ecoin: this buffer is excluded")
    (ecoin--request 'manual)))

(defconst ecoin--own-commands
  '(ecoin-accept ecoin-accept-word ecoin-accept-line ecoin-next ecoin-previous ecoin-complete))

(defconst ecoin--prefix-commands
  '(universal-argument digit-argument negative-argument universal-argument-more)
  "Commands that build a prefix argument and must not dismiss the ghost.")

(defun ecoin--keeps-ghost-p ()
  "Non-nil if the running command is ecoin's own or builds a prefix argument."
  (or (memq this-command ecoin--own-commands)
      (memq this-command ecoin--prefix-commands)
      (memq this-original-command ecoin--prefix-commands)))

;;;; Triggering

(defun ecoin--cancel-timer ()
  (when ecoin--timer (cancel-timer ecoin--timer) (setq ecoin--timer nil)))

(defun ecoin--idle (buf trigger)
  (when (and (buffer-live-p buf) (eq buf (current-buffer)))
    (setq ecoin--timer nil)
    (when (ecoin--allowed-p) (ecoin--request trigger))))

(defun ecoin--pre-command ()
  "Hide the ghost before commands that read point's screen position.
Motion computed with the ghost on screen lands in the wrong column."
  (unless (or (ecoin--keeps-ghost-p) (eq this-command 'self-insert-command))
    (ecoin--clear-overlay)))

(defun ecoin--post-command ()
  (ecoin--cancel-timer)
  (let* ((changed (not (eq ecoin--last-tick (buffer-chars-modified-tick))))
         (moved (not (eql ecoin--last-point (point))))
         (trigger (cond (changed 'auto)
                        ((and moved (ecoin--trigger-on-move-p)) 'move))))
    (setq ecoin--last-tick (buffer-chars-modified-tick)
          ecoin--last-point (point))
    (cond
     ((not (ecoin--may-show-p)) (ecoin--clear-overlay))
     ((ecoin--keeps-ghost-p) nil)
     ((ecoin--typed-into-ghost) nil)
     (t
      (ecoin--clear-overlay)
      (when (and trigger ecoin-idle-delay (ecoin--allowed-p))
        (setq ecoin--timer (run-with-idle-timer ecoin-idle-delay nil
                                                #'ecoin--idle (current-buffer) trigger)))))))

(defun ecoin--on-window-change (&rest _)
  "Hide the ghost when the selected window changes."
  (ecoin--clear-overlay))

(defun ecoin--hide (&rest _)
  "Hide the ghost and forget the request in flight."
  (ecoin--cancel-timer)
  (ecoin--cancel-pending)
  (ecoin--clear-overlay))

;;;; Commands that dispatch to the backend

(defun ecoin--fallback-status ()
  "Describe the fallback backend without starting it, or return nil if none."
  (let ((fallback ecoin-fallback-backend))
    (when (and fallback (not (eq fallback ecoin-backend)))
      (let ((feature (alist-get fallback ecoin--backend-features)))
        (format "fallback %s: %s" fallback
                (cond ((eq (ecoin--fallback-in-use) fallback) "in use")
                      ((and feature (not (featurep feature))) "not started")
                      (t "standing by")))))))

(defun ecoin-status ()
  "Show the selected backend's status and whether the fallback is in use."
  (interactive)
  (ecoin--ensure-backend ecoin-backend)
  (message "ecoin: %s" (string-join
                        (delq nil (list (ecoin-backend-status ecoin-backend)
                                        (ecoin--fallback-status)))
                        "; ")))

(defun ecoin-restart ()
  "Restart the selected backend's server; it starts again on the next request."
  (interactive)
  (ecoin--ensure-backend ecoin-backend)
  (ecoin-backend-restart ecoin-backend))

;;;; Minor mode

(defun ecoin--lighter ()
  "Mode-line text of `ecoin-mode': the name plus the backend's marker."
  (concat " ecoin"
          (if-let* ((fallback (ecoin--fallback-in-use)))
              (format "[%s]" (or (alist-get fallback ecoin--backend-markers)
                                 fallback))
            (or (ignore-errors (ecoin-backend-mode-line ecoin-backend)) ""))))

(defun ecoin--disable-buffer ()
  "Tell every backend that served this buffer, and the current one, it is closed."
  (let ((backends (cl-adjoin ecoin-backend ecoin--served-by)))
    (setq ecoin--served-by nil)
    (dolist (backend backends)
      (ecoin--hook #'ecoin-backend-disable-buffer backend))))

;;;###autoload
(define-minor-mode ecoin-mode
  "Show inline suggestions from `ecoin-backend' as ghost text."
  :lighter (:eval (ecoin--lighter))
  (if ecoin-mode
      (progn
        (setq ecoin--last-tick (buffer-chars-modified-tick)
              ecoin--last-point (point))
        (add-hook 'pre-command-hook #'ecoin--pre-command nil t)
        (add-hook 'post-command-hook #'ecoin--post-command nil t)
        (add-hook 'window-selection-change-functions #'ecoin--on-window-change nil t)
        (when (boundp 'evil-insert-state-exit-hook)
          (add-hook 'evil-insert-state-exit-hook #'ecoin--hide nil t))
        (add-hook 'kill-buffer-hook #'ecoin--disable-buffer nil t)
        (cl-pushnew ecoin-backend ecoin--served-by)
        (ecoin--hook #'ecoin-backend-enable-buffer ecoin-backend))
    (ecoin--cancel-timer)
    (ecoin--cancel-pending)
    (ecoin--clear-overlay)
    (remove-hook 'pre-command-hook #'ecoin--pre-command t)
    (remove-hook 'post-command-hook #'ecoin--post-command t)
    (remove-hook 'window-selection-change-functions #'ecoin--on-window-change t)
    (when (boundp 'evil-insert-state-exit-hook)
      (remove-hook 'evil-insert-state-exit-hook #'ecoin--hide t))
    (remove-hook 'kill-buffer-hook #'ecoin--disable-buffer t)
    (ecoin--disable-buffer)))

(defun ecoin--turn-on ()
  (when (and (derived-mode-p 'prog-mode 'text-mode 'conf-mode)
             (not (minibufferp))
             (not buffer-read-only))
    (ecoin-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-ecoin-mode ecoin-mode ecoin--turn-on)

(provide 'ecoin)
;;; ecoin.el ends here
