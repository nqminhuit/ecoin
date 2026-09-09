# Setup

```
npm install -g @github/copilot-language-server
```

```elisp
(add-to-list 'load-path "/path/to/ecoin")
(require 'ecoin)
(add-hook 'prog-mode-hook #'ecoin-mode)   ; or (global-ecoin-mode)
```
Then `M-x ecoin-login` once.

**Keys (only active while ghost text is shown)**: `TAB` accept, `C-TAB` accept a word, `M-n`/`M-p` cycle alternatives; anything else dismisses. `ecoin-accept-line` exists but is unbound. Rebind in `ecoin-completion-map`.

**How it works, briefly**

- One global jsonrpc connection, started lazily on the first request.
- Full-buffer `didChange` right before each request (simple and can't drift out of sync).
- Idle timer fires after any edit; stale replies are dropped if point or buffer changed meanwhile.
- If you type the next character of the ghost text, it just shrinks instead of re-requesting (no flicker).
- Positions use UTF-16 columns, so emoji/CJK won't break it.
- Server-side telemetry (`didShowCompletion`, accept command, partial accept) is sent so GitHub counts your usage correctly.

**Other commands**: `ecoin-complete` (manual trigger), `ecoin-status`, `ecoin-logout`, `ecoin-restart`. Server logs go to `*ecoin-log*`, crashes to ` *ecoin-stderr*`.

**Known rough edges**: mid-line suggestions push the rest of the line after the ghost text (same as copilot.el); `ecoin--indent-width` guesses from common mode variables and falls back to `tab-width`; a "not signed in" message appears twice on first start (once from the server's status notification, once from ecoin).
