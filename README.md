# dotconfig

Personal dotfiles, managed with [chezmoi](https://chezmoi.io).

The bare-minimum toolset that gets installed and configured: **nushell**
(shell), **starship** (prompt), **rio** (terminal, set as the default terminal
where the OS allows it), **emacs** (editor, config cloned from a separate repo),
**git**, a set of modern CLI tools (ripgrep, fd, fzf, bat, eza, zoxide,
git-delta, jq), and the **Aporetic** font family.

Supported platforms: **macOS** (Homebrew), **Arch Linux** (pacman + paru),
**Windows** (winget).

## Bootstrap a fresh machine

**macOS / Arch Linux:**

```sh
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply https://github.com/felipebalbi/dotconfig.git
```

**Windows (PowerShell):**

```powershell
winget install twpayne.chezmoi
chezmoi init --apply https://github.com/felipebalbi/dotconfig.git
```

This installs chezmoi, clones this repo into chezmoi's source directory, installs
the packages, and applies the configuration.

## Repository layout

```
.chezmoiroot          # points chezmoi at home/ as the source root
home/                 # chezmoi source state
  dot_config/         # -> ~/.config/*
    rio/  nushell/  starship.toml   # all platforms
    niri/                           # Arch only (Wayland compositor)
    systemd/user/emacs.service      # Arch only (Emacs daemon unit)
  private_dot_ssh/    # -> ~/.ssh/ (0700); Arch Linux only
  dot_gitconfig       # -> ~/.gitconfig
  .chezmoidata/       # packages.yaml: per-OS package lists
  .chezmoiexternal.toml         # emacs.d clone + Aporetic fonts
  .chezmoiignore                # per-OS exclusions (niri/systemd/ssh are Arch-only)
  run_onchange_before_install-packages.sh.tmpl   # macOS/Arch package install
  run_onchange_before_install-packages.ps1.tmpl  # Windows package install
  run_once_after_set-defaults.sh.tmpl            # login shell, default terminal, emacs.service
legacy/               # all previously-used configs, preserved, unmanaged
docs/                 # design + planning docs
install.sh            # convenience wrapper around the macOS/Arch one-liner
install.ps1           # convenience wrapper around the Windows flow
```

## Day-to-day

- Edit a managed file: `chezmoi edit ~/.config/rio/config.toml`
- Preview changes: `chezmoi diff`
- Apply changes: `chezmoi apply`
- Pull latest + apply: `chezmoi update`
- Add a package: edit `home/.chezmoidata/packages.yaml`, then `chezmoi apply`
  (the install script re-runs automatically because its content changed).

## Notes

- **rio default terminal:** set via `$TERMINAL=rio` everywhere, plus
  `xdg-settings` on Arch. macOS has no system-wide default-terminal setting, and
  rio does not implement the Windows console-handoff API, so on those platforms
  rio is installed and used but not registered as an OS-level default.
- **Fonts:** the `nushell` aliases avoid glyph icons, but `starship` uses the
  Nerd Font symbols preset. Use a Nerd Font in your terminal for them to render
  (Aporetic itself is not patched with Nerd Font glyphs).
- Old window-manager and system configs live under `legacy/` and are not touched
  by chezmoi.
