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
| `install.sh` scope  | macOS (Homebrew) + Arch Linux (pacman + `paru` for AUR)                                                    |
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
    ├── run_onchange_install-packages.sh.tmpl   # unix: brew (macOS) / pacman+AUR (Arch)
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
  `sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply https://github.com/felipebalbi/dotconfig.git`
- **Windows (PowerShell):**
  `winget install twpayne.chezmoi; chezmoi init --apply https://github.com/felipebalbi/dotconfig.git`

(The explicit repo URL is required because `chezmoi init felipebalbi` would look
for a repo named `dotfiles`; this repository is `dotconfig`.)

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
- **Fonts:** Aporetic family fetched via `.chezmoiexternal.toml` from the
  `protesilaos/aporetic` git **tag tarball** (the project publishes no GitHub
  release assets), extracting the `.ttf` files into the platform font directory.
  Aporetic is **not** a Nerd Font, so starship/eza configs are kept glyph-free to
  avoid missing-glyph boxes.

Package managers: Homebrew (macOS), pacman + **paru** for AUR packages (Arch
Linux), winget (Windows). The Arch script bootstraps `paru` if it is missing,
then installs official-repo packages with `pacman` and AUR-only packages (e.g.
rio, possibly others) with `paru`.

## rio as the default terminal "everywhere"

Per-OS reality, stated honestly:

- **Shell-level (all OSes):** nushell `env.nu` exports `TERMINAL = "rio"`, so any
  tool honoring `$TERMINAL` launches rio.
- **Arch Linux:** `run_once` sets rio as the default terminal via
  `xdg-settings set default-terminal-emulator rio.desktop` (and `gsettings`
  where a GNOME-style setting applies). No `update-alternatives` — that is a
  Debian mechanism and Arch is the only supported distro.
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

- Migrating or maintaining any `legacy/` window-manager or system configs (other
  than those promoted in Revision 2 below).
- Non-Arch Linux distributions (Debian/apt, Fedora/dnf, etc.). Arch Linux is the
  only supported Linux distro; the Unix installer targets macOS + Arch.
- Secrets management (no secrets are currently tracked; can be added via chezmoi
  later if needed).
- vim, zsh configs (preserved in `legacy/`, not actively managed).

## Revision 2 (post-implementation amendments)

Added after the first whole-branch review and follow-up user requirements.

### Additional managed configs (promoted out of `legacy/`)

OS-conditional application is handled by a new templated `home/.chezmoiignore`
that ignores each config on the OSes where it does not apply.

| Source | Target | OSes |
|---|---|---|
| `home/dot_config/niri/config.kdl` | `~/.config/niri/config.kdl` | Arch Linux only |
| `home/private_dot_ssh/config` | `~/.ssh/config` (dir 0700) | macOS + Arch Linux (ignored on Windows) |
| `home/dot_config/systemd/user/emacs.service` | `~/.config/systemd/user/emacs.service` | Arch Linux only |

- **niri** is added to the Arch `paru` package list (it is in the `extra` repo),
  so a fresh Arch machine installs the compositor it configures.
- The **emacs user service** is enabled on Arch via
  `systemctl --user enable emacs.service` in `run_once_after_set-defaults.sh`
  (this is how Emacs is run as a daemon on the Arch box).

### macOS correctness fixes (from the whole-branch review)

- **nushell config location (was a blocker):** nushell's default config dir on
  macOS is `~/Library/Application Support/nushell`, not `~/.config/nushell`. The
  `run_once_after` script creates a symlink
  `~/Library/Application Support/nushell` → `~/.config/nushell` on macOS, mirroring
  the Windows `%APPDATA%\nushell` junction. (Linux is native XDG; no change.)
- **Homebrew PATH on Apple Silicon:** `/opt/homebrew/bin` is not on the default
  login PATH. Two fixes: (a) the `run_once_after` login-shell step falls back to
  absolute candidate paths (`/opt/homebrew/bin/nu`, `/usr/local/bin/nu`,
  `/usr/bin/nu`) when `nu` is not on PATH, so the `chsh` step does not silently
  no-op; (b) `nushell/env.nu` prepends Homebrew's bin dirs to `PATH` on macOS so
  brew-installed tools (starship, zoxide, eza, …) resolve in nushell sessions.
- `install.sh` keeps its executable bit (mode `100755`).

## Success criteria

## Success criteria

1. On a fresh macOS or Arch Linux machine, the Unix one-liner produces a working
   nushell + starship + rio + emacs + git environment with the modern CLI tools
   and Aporetic fonts installed.
2. On a fresh Windows machine, the PowerShell flow installs the same toolset via
   winget and applies the same config source tree.
3. `chezmoi apply` is idempotent and never touches unmanaged `~/.config` apps.
4. rio is the `$TERMINAL` everywhere, and the OS-level default terminal wherever
   the platform supports it (Arch Linux; documented limitations on macOS/Windows).
5. All previous configuration remains present under `legacy/`.
