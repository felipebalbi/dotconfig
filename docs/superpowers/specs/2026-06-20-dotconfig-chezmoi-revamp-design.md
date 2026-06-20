# dotconfig revamp — chezmoi-based design

**Date:** 2026-06-20
**Status:** Approved (design); pending implementation plan

## Goal

Modernize and rebuild the `dotconfig` repository so a fresh machine (macOS,
Linux, or Windows) can be set up to the "bare minimum I actually rely on" with a
single bootstrap command. Nothing currently in the repository is deleted;
unused configuration is preserved.

The actively-managed toolset:

- **nushell** — shell
- **starship** — prompt
- **rio** — terminal (and the default terminal everywhere it is achievable)
- **emacs** — editor (config lives in a separate repo, cloned on apply)
- **git** — `~/.gitconfig`
- **modern CLI tools** — ripgrep, fd, fzf, bat, eza, zoxide, git-delta, jq
- **fonts** — Aporetic family (rio uses *Aporetic Serif Mono*)

## Decisions (from brainstorming)

| Topic               | Decision                                                                                                   |
|---------------------|------------------------------------------------------------------------------------------------------------|
| Engine              | **chezmoi** (cross-platform, modern, single source tree for all OSes)                                      |
| Apply mode          | **Managed files** (chezmoi default), not symlink mode                                                      |
| Symlink granularity | Per-app effect: chezmoi only manages files present in the source; never touches unmanaged `~/.config` apps |
| Repo layout         | `home/` (chezmoi source via `.chezmoiroot`) + `legacy/` (preserved) + `docs/`                              |
| `install.sh` scope  | macOS (Homebrew) + Linux (apt/dnf/pacman auto-detect)                                                      |
| Windows             | `install.ps1` thin wrapper; chezmoi + winget                                                               |
| Bootstrap           | chezmoi official one-liner; repo cloned to chezmoi's default source dir                                    |
| Conflict handling   | chezmoi leaves unmanaged files alone; managed files overwritten on apply (preview via `chezmoi diff`)      |
| Package install     | chezmoi `run_onchange_` script driven by `.chezmoidata/packages.yaml`                                      |
| Emacs config        | cloned from `github.com/felipebalbi/emacs.d` via chezmoi external                                          |
| Default login shell | nushell set via `run_once` on macOS/Linux (N/A Windows)                                                    |

## Repository layout

```
dotconfig/
├── .chezmoiroot                 # one line: "home"  → chezmoi source root = home/
├── README.md                    # rewritten: bootstrap one-liners + how it works
├── docs/superpowers/specs/      # this design doc
├── legacy/                      # ALL current configs preserved, untouched by chezmoi
│   ├── alacritty/ foot/ hypr/ niri/ nyxt/ picom/ rofi/ stumpwm/
│   ├── sway/ waybar/ xmonad/ X/ vim/ zsh/ ssh/ guix/ defconfig/
│   ├── systemd/ udev/ nfs/ latexmkrc signature
│   └── install.sh               # old installer, kept for reference
└── home/                        # chezmoi source root
    ├── .chezmoidata/packages.yaml          # per-OS package lists
    ├── .chezmoiexternal.toml               # emacs.d clone + fonts download
    ├── .chezmoiignore                      # ignore wrong-OS files
    ├── dot_config/
    │   ├── rio/config.toml.tmpl            # templated shell path per-OS
    │   ├── rio/themes/catppuccin-mocha.toml
    │   ├── nushell/config.nu.tmpl
    │   ├── nushell/env.nu.tmpl             # TERMINAL=rio, starship + zoxide init
    │   └── starship.toml                   # → ~/.config/starship.toml
    ├── dot_gitconfig                       # → ~/.gitconfig (from git/gitconfig)
    ├── run_onchange_install-packages.sh.tmpl   # unix: brew/apt/dnf/pacman
    ├── run_onchange_install-packages.ps1.tmpl  # windows: winget
    └── run_once_after_set-defaults.sh.tmpl     # default login shell + default terminal
```

The old top-level directories are moved wholesale into `legacy/`. No file is
deleted. Promoting any legacy config later is a move into `home/` plus chezmoi
renaming (`dot_` prefixes etc.).

## Migration map

| Current           | New                                     | Destination on `chezmoi apply`            |
|-------------------|-----------------------------------------|-------------------------------------------|
| `rio/config.toml` | `home/dot_config/rio/config.toml.tmpl`  | `~/.config/rio/config.toml`               |
| `rio/themes/`     | `home/dot_config/rio/themes/`           | `~/.config/rio/themes/`                   |
| `git/gitconfig`   | `home/dot_gitconfig`                    | `~/.gitconfig`                            |
| *(new)* nushell   | `home/dot_config/nushell/`              | `~/.config/nushell/`                      |
| *(new)* starship  | `home/dot_config/starship.toml`         | `~/.config/starship.toml`                 |
| *(new)* emacs     | `home/.chezmoiexternal.toml` (git-repo) | `~/.config/emacs` ← `felipebalbi/emacs.d` |
| everything else   | `legacy/…`                              | not touched                               |

## Bootstrap

The fresh-machine one-liners (the core deliverable):

- **macOS / Linux:**
  `sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply felipebalbi`
- **Windows (PowerShell):**
  `winget install twpayne.chezmoi; chezmoi init --apply felipebalbi`

`install.sh` and `install.ps1` at the repo root are thin, memorable wrappers
around these. One command on a bare machine results in: chezmoi installed, repo
cloned to chezmoi's source dir (`~/.local/share/chezmoi` on Unix,
`%USERPROFILE%\.local\share\chezmoi` on Windows), configs applied, and packages
installed.

## Packages

`home/.chezmoidata/packages.yaml` holds the canonical lists; the per-OS
`run_onchange_install-packages.*` scripts resolve names and call the right
package manager. `run_onchange_` re-runs only when the package list content
changes (chezmoi hashes the rendered script).

- **Core:** nushell, starship, rio, emacs, git
- **Modern CLI:** ripgrep, fd, fzf, bat, eza, zoxide, git-delta, jq
- **Fonts:** Aporetic family fetched via `.chezmoiexternal.toml` from
  `protesilaos/aporetic` GitHub releases into the platform font directory
  (identical mechanism on every OS).

Package managers: Homebrew (macOS), apt/dnf/pacman auto-detected (Linux),
winget (Windows).

## rio as the default terminal "everywhere"

Per-OS reality, stated honestly:

- **Shell-level (all OSes):** nushell `env.nu` exports `TERMINAL = "rio"`, so any
  tool honoring `$TERMINAL` launches rio.
- **Linux:** `run_once` registers rio via
  `update-alternatives --set x-terminal-emulator` (Debian) and
  `xdg-settings` / `gsettings` where available.
- **macOS:** there is **no system-wide "default terminal" API** — applications do
  not share a default terminal setting. We ensure rio is installed and used and
  set `$TERMINAL`; the limitation is documented rather than worked around.
- **Windows:** the Windows 11 "Default terminal application" handoff requires a
  terminal to implement the console-handoff API. rio currently does **not**
  implement it, so this is likely not achievable. Documented; fallback is a
  Start-menu shortcut plus `$TERMINAL`.

## Templating

- `rio/config.toml.tmpl`: the hardcoded `/opt/homebrew/bin/nu` becomes a
  template that resolves the `nu` path per OS (e.g. `lookPath`), correct on
  macOS / Linux / Windows.
- nushell `env.nu.tmpl`: initializes starship and zoxide, exports `TERMINAL=rio`.

## Cross-platform path details to resolve during implementation

These are known wrinkles, not blockers:

- **nushell config dir on Windows** defaults to `%APPDATA%\nushell`, while
  chezmoi's `dot_config` maps to `~/.config`. Resolve by setting
  `XDG_CONFIG_HOME` (or nushell's config path) so Windows also reads
  `~/.config/nushell`, or by templating the destination per OS.
- **starship** reads `~/.config/starship.toml` on all platforms (Windows `$HOME`
  = `%USERPROFILE%`), so no special handling needed.
- **chezmoi script interpreters on Windows:** `.ps1` scripts run natively; the
  `.sh` package script is guarded with `{{ if ne .chezmoi.os "windows" }}` (and
  the `.ps1` with the inverse) and/or excluded via `.chezmoiignore` so only the
  correct one runs per OS.
- **Aporetic font install path** differs per OS (`~/Library/Fonts`,
  `~/.local/share/fonts`, `%LOCALAPPDATA%\Microsoft\Windows\Fonts`); handled by
  chezmoi external destination templating.

## Default login shell

`run_once_after_set-defaults.sh` adds nushell to `/etc/shells` (if missing) and
sets it as the login shell via `chsh` on macOS/Linux. Not applicable on Windows.

## Out of scope

- Migrating or maintaining any `legacy/` window-manager or system configs.
- Secrets management (no secrets are currently tracked; can be added via chezmoi
  later if needed).
- ssh, vim, zsh configs (preserved in `legacy/`, not actively managed).

## Success criteria

1. On a fresh macOS or Linux machine, the Unix one-liner produces a working
   nushell + starship + rio + emacs + git environment with the modern CLI tools
   and Aporetic fonts installed.
2. On a fresh Windows machine, the PowerShell flow installs the same toolset via
   winget and applies the same config source tree.
3. `chezmoi apply` is idempotent and never touches unmanaged `~/.config` apps.
4. rio is the `$TERMINAL` everywhere, and the OS-level default terminal wherever
   the platform supports it (Linux; documented limitations on macOS/Windows).
5. All previous configuration remains present under `legacy/`.
