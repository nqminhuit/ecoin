# Setup

ecoin has a pluggable backend, chosen with `ecoin-backend`: `llama` (the default; a local llama.cpp server's `/infill` endpoint, in `ecoin-llama.el`) or `copilot` (GitHub Copilot, in `ecoin-copilot.el`). Each is loaded on first use. A full README rewrite will follow once more backends exist.

```elisp
(add-to-list 'load-path "/path/to/ecoin")
(require 'ecoin)
(add-hook 'prog-mode-hook #'ecoin-mode)   ; or (global-ecoin-mode)
```

## llama backend (default)

Run a llama-server with a FIM-capable model, for example `llama-server --fim-qwen-1.5b-default`, then point ecoin at it:

```elisp
(setq ecoin-llama-url "http://127.0.0.1:8012"   ; http only
      ecoin-llama-api-key "~/.llama-key")       ; nil, a key, a file holding it, or a function
```

Only requests to `ecoin-llama-url` are made, and nothing is sent for files matching `ecoin-exclude-file-regexps`. If the server is down, asleep, loading, rejects the key or has no FIM tokens, ecoin shows one message, a mode-line marker (`ecoin[z]` sleeping, `ecoin[!]` failing) and backs off instead of failing on every keystroke; `M-x ecoin-status` shows the state and `M-x ecoin-restart` resets it. `ecoin-complete` asks for several alternatives (M-n / M-p), up to the server's slot count. Answers are cached (`ecoin-llama-cache-size`), so repeating or typing through a suggestion costs no request, and after a suggestion is shown ecoin asks ahead for the one that follows accepting it (`ecoin-llama-prefetch`, only within `ecoin-llama-activity-window` seconds of your last trigger).

## Fallback to another backend

```elisp
(setq ecoin-fallback-backend 'copilot)
```

While the primary backend is unavailable (for llama: unreachable, loading a model, no FIM tokens, key rejected or a configuration error, during its backoff), requests go to the fallback and ecoin says so once; when the primary answers again it says that once too, and the mode-line shows `ecoin[cp]` in between. A sleeping llama server is not unavailable: the request wakes it. **Setting this option is your consent to send code to that backend (GitHub, for Copilot) whenever the primary is unavailable**; there is no prompt. Excluded files are never sent. The default, nil, never falls back.

## Copilot backend

```
npm install -g @github/copilot-language-server
```

```elisp
(setq ecoin-backend 'copilot)
```
Then `M-x ecoin-login` once.

**Keys (only active while ghost text is shown)**: `TAB` accept, `C-TAB` accept a word, `M-n`/`M-p` cycle alternatives; anything else dismisses. `ecoin-accept-line` exists but is unbound. Rebind in `ecoin-completion-map`.

**How the Copilot backend works, briefly**

- One global jsonrpc connection, started lazily on the first request.
- Full-buffer `didChange` right before each request (simple and can't drift out of sync).
- Idle timer fires after any edit; stale replies are dropped if point or buffer changed meanwhile.
- If you type the next character of the ghost text, it just shrinks instead of re-requesting (no flicker).
- Positions use UTF-16 columns, so emoji/CJK won't break it.
- Server-side telemetry (`didShowCompletion`, accept command, partial accept) is sent so GitHub counts your usage correctly.

**Other commands**: `ecoin-complete` (manual trigger), `ecoin-status`, `ecoin-logout`, `ecoin-restart`. Server logs go to `*ecoin-log*`, crashes to ` *ecoin-stderr*`.

**Known rough edges**: mid-line suggestions push the rest of the line after the ghost text (same as copilot.el); `ecoin--indent-width` guesses from common mode variables and falls back to `tab-width`.

**The server hangs if `~/.config/github-copilot/github` exists when it starts** — it answers `initialize` and then never replies again. A directory or a plain file does it, empty or not; only the state at startup matters, and the server recreates the directory during normal use, harmlessly. Reproduced with a standalone LSP client on server versions 1.506.1 through 1.544.0, so downgrading does not help. `ecoin--connect` deletes that path before spawning, and leaves it alone if it ever holds real files — set `ecoin-purge-path-before-connect` to nil to opt out.
