# ecoin

Inline completions (ghost text) for Emacs, with pluggable backends. ecoin shows a suggestion after you stop typing, and nothing else: no popup, no chat, no side windows. Needs Emacs 28.1 or newer.

Two backends ship with it, each loaded only when used:

- `llama` (the default): a local [llama.cpp](https://github.com/ggml-org/llama.cpp) server's `/infill` endpoint, with any model trained for fill-in-the-middle (FIM). Your code goes only to the URL you configure.
- `copilot`: GitHub Copilot through the official `copilot-language-server`.

## Install

ecoin is not on MELPA. Clone it and add it to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/ecoin")
(require 'ecoin)
(global-ecoin-mode)   ; or (add-hook 'prog-mode-hook #'ecoin-mode)
```

`global-ecoin-mode` turns the mode on in every writable `prog-mode`, `text-mode` and `conf-mode` buffer.

With Doom Emacs, in `packages.el`:

```elisp
(package! ecoin :recipe (:host github :repo "nqminhuit/ecoin"))
```

and in `config.el`:

```elisp
(use-package! ecoin
  :config
  (setq ecoin-backend 'llama
        ecoin-fallback-backend 'copilot      ; optional, see "Fallback"
        ecoin-llama-url "http://127.0.0.1:8012"
        ecoin-llama-api-key "~/.llama-key")  ; omit if the server has no key
  (global-ecoin-mode))
```

## llama backend

Start a llama-server with a FIM-capable model. The quickest way is a built-in preset:

```
llama-server --fim-qwen-1.5b-default
```

For a dedicated server (for example a container) that you leave running, these flags suit ecoin: a single slot, so the KV cache stays warm for the buffer you are typing in, and prompt-cache reuse for the shifted prefix.

```
llama-server ... -c 16384 -np 1 -b 2048 -ub 1024 --cache-reuse 256
```

Then point ecoin at it:

```elisp
(setq ecoin-llama-url "http://127.0.0.1:8012"   ; http only
      ecoin-llama-api-key "~/.llama-key")       ; nil, a key, a file holding it, or a function
```

ecoin checks `/props`, so it notices a server that is down, asleep, still loading, rejecting the key, or running a model without FIM tokens. Each of those costs one message, a mode-line marker and a backoff, not an error per keystroke. A sleeping server (the `/props` `is_sleeping` flag) is woken by the next request, which then uses a long timeout.

A llama.cpp router (`llama-server --models-preset ...`) serves several models and picks one per request by its name, so set `ecoin-llama-model` to the preset name, for example `(setq ecoin-llama-model "qwen2.5-coder-1.5b-q8_0.gguf")`. ecoin then names the model in every request and checks its `/props` without loading it. The first request after switching waits for the model to load (about 1-6 s on a GPU); ecoin may briefly use the fallback meanwhile. Changing the option clears the cached completions. With the default `nil`, ecoin sends no model, which is what a single-model server expects; against a router it reports that `ecoin-llama-model` is missing.

Behaviour worth knowing:

- Answers are cached, so repeating a request or typing through a suggestion costs no request.
- After a ghost is shown, ecoin asks ahead for the one that follows accepting it (`ecoin-llama-prefetch`). Background requests are sent only within `ecoin-llama-activity-window` seconds of your last trigger, so an idle server can go back to sleep.
- At most one request is in flight; a newer trigger replaces an older one that is still queued.
- `M-x ecoin-complete` asks for several alternatives (up to `ecoin-llama-manual-alternatives` and the server's slot count); cycle them with `M-n` and `M-p`.
- Besides the text around point, each request carries extra context from other files of the same project (files you visit or save, text you copy, parts of the buffer far from point), kept in a small per-project ring. It never crosses projects, and excluded files never contribute. Turn it off by setting `ecoin-llama-ring-chunks` to 0.

### Main options

| Option | Default | Meaning |
| --- | --- | --- |
| `ecoin-llama-url` | `"http://127.0.0.1:8012"` | Server root; http only. A non-loopback host triggers a one-time warning (`ecoin-llama-warn-non-loopback`). |
| `ecoin-llama-api-key` | `nil` | Key, key file name, or function returning the key. |
| `ecoin-llama-model` | `nil` | Model id a llama.cpp router routes by; `nil` sends none (single-model server). |
| `ecoin-llama-n-prefix` | 256 | Lines above the cursor line sent as context. |
| `ecoin-llama-n-suffix` | 64 | Lines below the cursor line sent as context. |
| `ecoin-llama-n-predict` | 128 | Maximum tokens generated. |
| `ecoin-llama-t-max-predict-ms` | 250 | After this, the server stops at the next newline. |
| `ecoin-llama-cache-size` | 250 | Contexts whose completions are cached, least recently used out. |
| `ecoin-llama-prefetch` | `t` | Ask ahead for the continuation after accepting. |
| `ecoin-llama-activity-window` | 30 | Seconds after your last trigger during which background requests are sent. |
| `ecoin-llama-ring-chunks` | 16 | Extra-context chunks kept per project; 0 turns it off. |
| `ecoin-llama-extra-max-chars` | 24000 | Cap on the extra context of one request; the oldest chunks go first. |

## Copilot backend

```
npm install -g @github/copilot-language-server
```

```elisp
(setq ecoin-backend 'copilot)
```

Then `M-x ecoin-login` once (`M-x ecoin-logout` signs out).

## Fallback

```elisp
(setq ecoin-fallback-backend 'copilot)
```

While the primary backend is unavailable (for llama: unreachable, loading a model, no FIM tokens, key rejected, or a configuration error, during its backoff), requests go to the fallback and ecoin says so once. When the primary answers again ecoin says that once too. In between, the mode line shows `[cp]`. A sleeping llama server is not unavailable: the request wakes it.

**Setting this option is your consent to send code to the fallback backend (GitHub, for Copilot) whenever the primary is unavailable.** There is no prompt. The default, `nil`, never falls back, so with llama alone your code goes only to the server you configured.

## Privacy and exclusions

- The llama backend contacts only `ecoin-llama-url`. It sends the text around point and the project's extra context. ecoin writes nothing to disk.
- Buffers visiting files that match `ecoin-exclude-file-regexps` get no suggestions and are never sent to any backend, the fallback included, nor used as extra context. The default covers `.env*`, `.authinfo*`, `.netrc`, `*.gpg`, `*.age`, `*.pem`, `*.key`, `.ssh`, `.aws`, `.gnupg` and `.password-store` directories, `id_rsa` and `id_ed25519` files, and `secrets.{yml,yaml,json,env}`.
- `ecoin-exclude-functions` is a hook: functions called with no arguments in the buffer; if any returns non-nil the buffer is excluded. If a check itself fails, the buffer counts as excluded.
- `*ecoin-log*` holds no buffer text unless you set `ecoin-log-content` to non-nil.
- Statistics (below) live in memory only.

## Keys

While a ghost is visible (bound in `ecoin-completion-map`):

| Key | Command |
| --- | --- |
| `TAB`, `<tab>` | `ecoin-accept`: accept the whole suggestion |
| `C-TAB`, `C-<tab>` | `ecoin-accept-word`: accept the next word |
| `M-n` / `M-p` | `ecoin-next` / `ecoin-previous`: cycle alternatives |
| (unbound) | `ecoin-accept-line`: accept the next line |

Any other key dismisses the ghost, except that typing the ghost's next character shrinks it. `M-x ecoin-complete` requests a suggestion at point. Rebind in `ecoin-completion-map`:

```elisp
(define-key ecoin-completion-map (kbd "C-.") #'ecoin-accept-word)
(define-key ecoin-completion-map (kbd "M-l") #'ecoin-accept-line)
```

`C-TAB` rarely reaches Emacs in a terminal, because most terminals send the same byte for it as for `TAB`; bind `ecoin-accept-word` to another key there, such as `C-.` above.

With evil, ecoin acts in insert and emacs state only (`ecoin-enable-predicates`); in normal state it makes no requests.

## Status, statistics and logs

- Mode line: `ecoin[z]` the llama server is asleep, `ecoin[!]` it is failing, `ecoin[cp]` Copilot is serving as the fallback.
- `M-x ecoin-status`: backend, state, URL, model, slots, last error and remaining backoff.
- `M-x ecoin-restart`: forget the llama server's state and backoff, or restart the Copilot server.
- `M-x ecoin-stats`: a read-only `*ecoin-stats*` buffer, kept in memory only, over the last 500 events, per backend:
  - median and 95th-percentile time from the request to the ghost (the idle delay before the request is not included);
  - ghosts shown, accepted and the acceptance rate (accepted divided by shown). A ghost counts once, by its first accept: full or typed through to the end, or partial by word or line;
  - ghosts dismissed (cleared without any accept);
  - for llama also the cache hit rate (exact, typed-through, misses), prefetch and warm-up counts, and the median and p95 of the server's `prompt_n`, `cache_n`, `prompt_ms`, `predicted_n` and `predicted_ms`.
- `M-x ecoin-stats-reset` clears the statistics.
- `*ecoin-log*`: backend and server events. The Copilot server's stderr goes to ` *ecoin-stderr*`.

## Copilot notes

- One global JSON-RPC connection, started on the first request; the full buffer is sent right before each request; positions use UTF-16 columns, so emoji and CJK are fine. Telemetry (`didShowCompletion`, accept command, partial accept) is sent so GitHub counts your usage.
- Mid-line suggestions push the rest of the line after the ghost text.
- The server hangs if `~/.config/github-copilot/github` (under `$XDG_CONFIG_HOME` if set) exists when it starts: it answers `initialize` and then never replies again. A directory or a plain file does it, empty or not; only the state at startup matters, and the server recreates the directory in normal use, harmlessly. Reproduced on server versions 1.506.1 through 1.544.0, so downgrading does not help. `ecoin--connect` deletes that path before spawning, and leaves it alone if it holds real files. Set `ecoin-purge-path-before-connect` to nil to opt out.

## Development

```
emacs -Q --batch -L . -L test -l test/ecoin-test.el -f ert-run-tests-batch-and-exit
```

Likewise for `test/ecoin-llama-test.el` and `test/ecoin-copilot-test.el`; run each in its own Emacs. The llama tests use a fake local server, so no GPU or model is needed.

## Licence

Apache-2.0, see [LICENSE](LICENSE).
