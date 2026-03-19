# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

A Doom Emacs private configuration (~/.doom.d). All custom modules live under `modules/` and follow Doom's module conventions. The Doom Emacs framework itself is checked out at `~/.emacs.d`.

## Essential Commands

```shell
# After changing init.el or packages.el (adding/removing modules or packages)
doom sync

# After changing config.el or module config files only
doom sync -u   # or just restart Emacs

# Byte-compile for performance (optional)
doom compile

# Diagnose issues
doom doctor
```

There is no test suite or linter — validation is done by loading the config in Emacs.

## Architecture

### Module Loading Order

Doom loads each module in three phases: `init.el` → `config.el` → keybinds. Within each module:

1. **packages.el** — declares packages via `package!`
2. **init.el** — early initialization (runs before other modules' config)
3. **autoload/*.el** or **autoload.el** — lazily-loaded functions (available everywhere but loaded on first call)
4. **config.el** — main configuration (runs after all modules' init.el)

The root `init.el` controls which modules are active via `doom!`. The root `config.el` holds cross-cutting settings that don't belong to a specific module.

### Custom Module Map

All custom modules are prefixed `zc-` and live in `modules/`:

| Module          | Category  | Purpose                                                                  |
|-----------------|-----------|--------------------------------------------------------------------------|
| `zc-prelude`    | `:config` | Bootstraps paths, loads `lisp/` libraries (cache, misc, secret)          |
| `zc-keybinds`   | `:config` | All custom keybindings (~340 lines) — the single source for key mappings |
| `zc-evil`       | `:editor` | Evil state cursors, escape, iedit, expand-region                         |
| `zc-theme`      | `:ui`     | Fonts (Fira Code/Sans + CJK), custom dracula theme, visual polish        |
| `zc-modeline`   | `:ui`     | Doom modeline segments: workspace, window number, eldoc                  |
| `zc-workspaces` | `:ui`     | Eyebrowse-based window layouts                                           |
| `zc-org`        | `:lang`   | Org directory (`~/notes`), babel, agenda, refile — heavy autoloads       |
| `zc-typescript` | `:lang`   | TS/TSX with tree-sitter, prettier formatter                              |
| `zc-llm`        | `:tools`  | gptel with Copilot, Claude, DeepSeek backends; auth via authinfo         |
| `zc-magit`      | `:tools`  | Repo discovery, fullframe, JIRA ticket prefix in commits                 |
| `zc-projectile` | `:tools`  | Workspace detection (yarn/npm), consult integration                      |
| `zc-lsp`        | `:tools`  | Eglot configuration and code-action indicators                           |

### Key Patterns

- **`after!`** (alias for `with-eval-after-load`) — defers config until a package loads. Almost all module config is wrapped in this.
- **`map!`** — Doom's keybinding macro with evil state awareness. All bindings consolidated in `zc-keybinds`.
- **`use-package!`** — Doom-flavored use-package for declaring package config with `:init`, `:config`, `:commands`.
- **Autoload convention** — functions prefixed `zc/` in `autoload.el` files are available globally without explicit require.

### Local Libraries (`lisp/`)

Loaded by `zc-prelude` at startup:

- **cache.el** — EIEIO-based TTL cache (`zc/cached` macro for memoization)
- **secret.el** — Auth-source wrappers (`zc/secrets-api-key`, `zc/secrets-basic-auth`)
- **misc.el** — Small utilities (childframe detection, timing macro, kill-emacs helper)

### Secrets

API keys and credentials are stored in `~/.authinfo.gpg` and accessed via `zc/secrets-api-key` or `zc/secrets-basic-auth`. Never hardcode credentials — use auth-source integration.

### Snippets

YASnippet templates in `snippets/` — currently covers `scala-mode` and `terraform-mode`.

### Legacy

`legacy/` contains old pre-Doom config files kept for reference. Do not modify or rely on these.
