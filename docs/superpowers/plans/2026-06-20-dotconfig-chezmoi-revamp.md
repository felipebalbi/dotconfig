# dotconfig chezmoi Revamp Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rebuild the `dotconfig` repo as a chezmoi source tree so a fresh macOS, Arch Linux, or Windows machine reaches a working nushell + starship + rio + emacs + git + modern-CLI environment with one bootstrap command, while preserving all old configs under `legacy/`.

**Architecture:** chezmoi (managed-files mode) drives everything. The repo root holds `.chezmoiroot` pointing at `home/`, which is the chezmoi source. `home/` uses chezmoi naming (`dot_config/`, `dot_gitconfig`), a `.chezmoidata/packages.yaml` data file, OS-templated `run_onchange_before_*` package-install scripts, a `.chezmoiexternal.toml` for the emacs.d clone + Aporetic fonts, and a `run_once_after_*` defaults script. Bootstrap uses chezmoi's official installer with the explicit repo URL.

**Tech Stack:** chezmoi, Go templates, bash, PowerShell, Homebrew (macOS), pacman + paru (Arch), winget (Windows), nushell, starship, rio.

## Global Constraints

- **Engine:** chezmoi, managed-files mode (NOT symlink mode).
- **chezmoi source root:** `home/` (selected by a `.chezmoiroot` file containing exactly `home`).
- **Supported OSes:** macOS (Homebrew), Arch Linux only (pacman + `paru` for AUR), Windows (winget). No apt/dnf/other distros.
- **Repo URL for bootstrap:** `https://github.com/felipebalbi/dotconfig.git` (explicit; `chezmoi init felipebalbi` would wrongly look for a `dotfiles` repo).
- **GitHub user / git identity:** `felipebalbi`, name `Felipe Balbi`, email `felipe@balbi.sh`.
- **emacs.d repo:** `https://github.com/felipebalbi/emacs.d.git`, cloned to `~/.config/emacs`.
- **Aporetic fonts:** pinned tag `1.2.0`, source `https://github.com/protesilaos/aporetic/archive/refs/tags/1.2.0.tar.gz` (no release assets exist). `.ttf` only. Aporetic is NOT a Nerd Font → keep starship/eza configs glyph-free.
- **SAFETY — never mutate the real machine during implementation:**
  - Never run a real `chezmoi apply` against `$HOME`. Always use `--dry-run` and a throwaway `--destination` (e.g. `--destination "$(mktemp -d)"`).
  - Never execute the `run_onchange_*` package scripts or the `run_once_*` defaults script for real (they install software and change the login shell). Only render them with `chezmoi execute-template` and lint them.
- **Default branch:** `master` (this repo's existing default).
- **Commit style:** short conventional-commit subject lines (e.g. `feat:`, `docs:`, `chore:`), matching existing history.
- Verification tooling: `chezmoi` (installed in Task 2), `shellcheck`, `python3` (TOML/YAML sanity), all available or installed via Homebrew on the macOS dev box.

---

### Task 1: Move unused configs into `legacy/`

Preserve every currently-unused config by moving it under `legacy/`. Leave `rio/` and `git/` in place (they migrate into `home/` in later tasks), plus repo meta files (`.git`, `.gitignore`, `README.md`, `docs/`).

**Files:**
- Create: `legacy/` (directory, via git mv targets)
- Move (git mv): `alacritty foot guix hypr niri nyxt picom rofi stumpwm sway waybar xmonad X vim zsh ssh systemd udev nfs defconfig latexmkrc signature install.sh` → `legacy/`

- [ ] **Step 1: Verify the starting tree**

Run: `ls -1`
Expected: includes `alacritty foot git rio zsh ... install.sh README.md docs`.

- [ ] **Step 2: Create legacy dir and move unused configs**

```bash
mkdir -p legacy
git mv alacritty foot guix hypr niri nyxt picom rofi stumpwm sway waybar xmonad X vim zsh ssh systemd udev nfs defconfig latexmkrc signature install.sh legacy/
```

- [ ] **Step 3: Verify nothing was lost and only intended dirs remain at root**

Run: `ls -1` and `ls -1 legacy`
Expected root: `README.md  docs  git  legacy  rio` (plus dotfiles `.git`, `.gitignore`).
Expected `legacy/`: all 22 moved entries present.

- [ ] **Step 4: Confirm git tracked the moves as renames**

Run: `git status --short`
Expected: lines beginning with `R` (renames) for every moved path; no `D`/`??` for moved content.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "chore: move unused configs into legacy/"
```

---

### Task 2: chezmoi source skeleton + dev tooling

Create the `.chezmoiroot` redirect and the empty `home/` source root, and install chezmoi + shellcheck for verification.

**Files:**
- Create: `.chezmoiroot`
- Create: `home/.keep` (placeholder so the empty dir is committed; removed once real files land — keep it until Task 4 adds content, then it's fine to leave or delete)

**Interfaces:**
- Produces: a valid chezmoi source. Later tasks invoke `chezmoi --source "$PWD" <cmd>`, which reads `.chezmoiroot` and uses `home/` as the source directory.

- [ ] **Step 1: Install chezmoi and shellcheck (dev box, macOS)**

Run: `brew install chezmoi shellcheck && chezmoi --version`
Expected: prints a chezmoi version (e.g. `chezmoi version v2.x`).

- [ ] **Step 2: Write `.chezmoiroot`**

File `/.chezmoiroot` (exact contents, single line + newline):

```
home
```

- [ ] **Step 3: Create the source root placeholder**

```bash
mkdir -p home
: > home/.keep
```

- [ ] **Step 4: Verify chezmoi recognizes the source root**

Run: `chezmoi --source "$PWD" source-path`
Expected: prints the absolute path ending in `/home` (proves `.chezmoiroot` is honored).

- [ ] **Step 5: Verify nothing is managed yet**

Run: `chezmoi --source "$PWD" managed`
Expected: empty output (no managed entries yet) and exit code 0.

- [ ] **Step 6: Commit**

```bash
git add .chezmoiroot home/.keep
git commit -m "feat: add chezmoi source skeleton (.chezmoiroot + home/)"
```

---

### Task 3: Migrate git config → `home/dot_gitconfig`

Move the gitconfig into the chezmoi source and fix the two hardcoded Linux-only paths so it works on macOS/Arch/Windows.

**Files:**
- Move (git mv): `git/gitconfig` → `home/dot_gitconfig`
- Modify: `home/dot_gitconfig` (lines `gpg.program` and `sendemail.aliasesfile`)

**Interfaces:**
- Produces: `~/.gitconfig` on apply.

- [ ] **Step 1: Move the file into the source tree**

```bash
git mv git/gitconfig home/dot_gitconfig
rmdir git 2>/dev/null || true
```

- [ ] **Step 2: Fix the gpg program path (use PATH lookup, not `/usr/bin/gpg`)**

In `home/dot_gitconfig`, change:

```
[gpg]
	program = /usr/bin/gpg
```

to:

```
[gpg]
	program = gpg
```

- [ ] **Step 3: Fix the send-email aliases path (absolute `/home/balbi` → `~`)**

In `home/dot_gitconfig`, change:

```
	aliasesfile = /home/balbi/.aliases
```

to:

```
	aliasesfile = ~/.aliases
```

- [ ] **Step 4: Verify chezmoi maps it to ~/.gitconfig with the fixes applied**

Run: `chezmoi --source "$PWD" cat ~/.gitconfig`
Expected: prints the full gitconfig; contains `program = gpg` and `aliasesfile = ~/.aliases`; does NOT contain `/usr/bin/gpg` or `/home/balbi`.

- [ ] **Step 5: Verify a dry-run apply targets the right path**

Run: `chezmoi --source "$PWD" --destination "$(mktemp -d)" apply --dry-run --verbose`
Expected: shows it would write `.gitconfig` (and no errors).

- [ ] **Step 6: Commit**

```bash
git add -A
git commit -m "feat: migrate gitconfig to chezmoi (home/dot_gitconfig), fix linux-only paths"
```

---

### Task 4: Migrate rio → `home/dot_config/rio/` with templated shell path

Move rio config + theme into the source, and turn the hardcoded `/opt/homebrew/bin/nu` shell program into a cross-platform template.

> **Design note (do not "fix" the fallback):** the template resolves `nu`'s
> absolute path via `lookPath`, but falls back to the bare string `"nu"` when
> `lookPath` returns empty. This makes the config correct regardless of whether
> `nu` is installed at the moment the template is rendered — rio launches `nu`
> from PATH in the fallback case, and a later `chezmoi apply`/`update` will fill
> in the absolute path. The fallback is intentional, not an oversight.


**Files:**
- Move (git mv): `rio/themes/catppuccin-mocha.toml` → `home/dot_config/rio/themes/catppuccin-mocha.toml`
- Move + rename (git mv): `rio/config.toml` → `home/dot_config/rio/config.toml.tmpl`
- Modify: `home/dot_config/rio/config.toml.tmpl` (templated `[shell] program`)
- Delete: `home/.keep` (no longer needed once real content exists)

**Interfaces:**
- Consumes: `nu` on PATH at apply time (installed by Task 8's `run_onchange_before_` script, which runs before files).
- Produces: `~/.config/rio/config.toml` and `~/.config/rio/themes/catppuccin-mocha.toml`.

- [ ] **Step 1: Move rio files into the source tree**

```bash
mkdir -p home/dot_config/rio/themes
git mv rio/themes/catppuccin-mocha.toml home/dot_config/rio/themes/catppuccin-mocha.toml
git mv rio/config.toml home/dot_config/rio/config.toml.tmpl
rmdir rio/themes rio 2>/dev/null || true
git rm -q home/.keep 2>/dev/null || rm -f home/.keep
```

- [ ] **Step 2: Replace the `[shell]` section with a templated program path**

Overwrite `home/dot_config/rio/config.toml.tmpl` with exactly:

```toml
{{- $nu := lookPath "nu" -}}
# See the full configuration reference: https://rioterm.com/docs/config

# Dark theme (loaded from ~/.config/rio/themes/catppuccin-mocha.toml)
theme = "catppuccin-mocha"

[shell]
program = {{ if $nu }}{{ $nu | quote }}{{ else }}"nu"{{ end }}
args = ["--login"]

[fonts]
size = 14
family = "Aporetic Serif Mono"
```

- [ ] **Step 3: Verify the template renders a valid program path**

Run: `chezmoi --source "$PWD" execute-template < home/dot_config/rio/config.toml.tmpl`
Expected: a TOML document where `program = "..."` is either an absolute path to `nu` (if `nu` is installed on the dev box) or `"nu"` (if not). No `{{ }}` left in the output.

- [ ] **Step 4: Verify the rendered output is valid TOML**

Run: `chezmoi --source "$PWD" execute-template < home/dot_config/rio/config.toml.tmpl | python3 -c "import sys,tomllib; tomllib.load(sys.stdin.buffer)"`
Expected: exit code 0, no exception.

- [ ] **Step 5: Verify chezmoi maps both files under ~/.config/rio**

Run: `chezmoi --source "$PWD" managed | grep rio`
Expected: lists `.config/rio/config.toml` and `.config/rio/themes/catppuccin-mocha.toml`.

- [ ] **Step 6: Commit**

```bash
git add -A
git commit -m "feat: migrate rio config to chezmoi with templated nu shell path"
```

---

### Task 5: nushell config (`env.nu`, `config.nu`)

Create a minimal modern nushell config: export `TERMINAL=rio`, generate starship + zoxide integrations into nushell's autoload dir, and add glyph-free aliases.

**Files:**
- Create: `home/dot_config/nushell/env.nu.tmpl`
- Create: `home/dot_config/nushell/config.nu.tmpl`

**Interfaces:**
- Consumes: `starship`, `zoxide`, `eza`, `bat` on PATH (installed by Task 8).
- Produces: `~/.config/nushell/env.nu` and `~/.config/nushell/config.nu`. On Windows these are made reachable by a junction created in Task 9.

- [ ] **Step 1: Write `home/dot_config/nushell/env.nu.tmpl`**

Exact contents:

```nu
# Managed by chezmoi.

# rio is the default terminal everywhere.
$env.TERMINAL = "rio"

# Pre-generate prompt/integration scripts into nushell's autoload dir.
# Files in this dir are sourced automatically on the next shell launch.
let autoload_dir = ($nu.data-dir | path join "vendor" "autoload")
mkdir $autoload_dir

if (which starship | is-not-empty) {
    starship init nu | save --force ($autoload_dir | path join "starship.nu")
}

if (which zoxide | is-not-empty) {
    zoxide init nushell | save --force ($autoload_dir | path join "zoxide.nu")
}
```

- [ ] **Step 2: Write `home/dot_config/nushell/config.nu.tmpl`**

Exact contents (no Nerd-Font glyphs — Aporetic has none):

```nu
# Managed by chezmoi.

$env.config.show_banner = false

# Modern CLI aliases (glyph-free; Aporetic is not a Nerd Font).
alias ll = eza -l --group-directories-first
alias la = eza -la --group-directories-first
alias lt = eza --tree
alias cat = bat --plain
```

- [ ] **Step 3: Verify both templates render without leftover template tags**

Run: `chezmoi --source "$PWD" execute-template < home/dot_config/nushell/env.nu.tmpl` and `chezmoi --source "$PWD" execute-template < home/dot_config/nushell/config.nu.tmpl`
Expected: both print their content verbatim (these files contain no template actions, so output equals input); exit code 0.

- [ ] **Step 4: Verify the rendered config loads in an isolated nushell (no real-home mutation)**

Only if `nu` is installed on the dev box; otherwise skip with a note.

```bash
TMP="$(mktemp -d)"
mkdir -p "$TMP/.config/nushell"
chezmoi --source "$PWD" execute-template < home/dot_config/nushell/env.nu.tmpl > "$TMP/.config/nushell/env.nu"
chezmoi --source "$PWD" execute-template < home/dot_config/nushell/config.nu.tmpl > "$TMP/.config/nushell/config.nu"
XDG_CONFIG_HOME="$TMP/.config" XDG_DATA_HOME="$TMP/.local/share" nu --no-history -c '$env.TERMINAL'
```

Run the block above.
Expected: prints `rio` with no config-load errors. (If `nu` is not installed, note "skipped — nu not present" and rely on Step 3.)

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat: add nushell config (TERMINAL=rio, starship/zoxide autoload, aliases)"
```

---

### Task 6: starship config

Add a clean, glyph-free starship prompt configuration.

**Files:**
- Create: `home/dot_config/starship.toml`

**Interfaces:**
- Produces: `~/.config/starship.toml` (read by starship on all OSes; `$HOME/.config` works on Windows too).

- [ ] **Step 1: Write `home/dot_config/starship.toml`**

Exact contents (no Nerd-Font symbols):

```toml
"$schema" = 'https://starship.rs/config-schema.json'

add_newline = true
command_timeout = 1000

format = """
$directory\
$git_branch\
$git_status\
$character"""

[directory]
truncation_length = 3
truncate_to_repo = true

[character]
success_symbol = "[>](bold green)"
error_symbol = "[>](bold red)"

[git_branch]
symbol = "git "
style = "bold purple"

[git_status]
style = "bold yellow"
```

- [ ] **Step 2: Verify it maps to ~/.config/starship.toml**

Run: `chezmoi --source "$PWD" managed | grep starship.toml`
Expected: prints `.config/starship.toml`.

- [ ] **Step 3: Verify it is valid TOML**

Run: `python3 -c "import tomllib; tomllib.load(open('home/dot_config/starship.toml','rb'))"`
Expected: exit code 0, no exception.

- [ ] **Step 4: Validate against starship if available**

Run: `command -v starship >/dev/null && STARSHIP_CONFIG="$PWD/home/dot_config/starship.toml" starship print-config >/dev/null && echo OK || echo "skipped/ok"`
Expected: prints `OK` (or `skipped/ok` if starship not installed); no parse error.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat: add glyph-free starship prompt config"
```

---

### Task 7: package data file (`.chezmoidata/packages.yaml`)

Declare the bare-minimum toolset with per-OS package identifiers. This data is consumed by the install scripts in Tasks 8–9.

**Files:**
- Create: `home/.chezmoidata/packages.yaml`

**Interfaces:**
- Produces template data accessible as `.packages.darwin.brews`, `.packages.darwin.casks`, `.packages.linux.paru`, `.packages.windows.winget`.

- [ ] **Step 1: Write `home/.chezmoidata/packages.yaml`**

Exact contents:

```yaml
# Bare-minimum toolset, per package-manager identifiers.
# Consumed by run_onchange_before_install-packages.{sh,ps1}.tmpl.
packages:
  # macOS — Homebrew
  darwin:
    brews:
      - nushell
      - starship
      - emacs
      - git
      - gnupg
      - ripgrep
      - fd
      - fzf
      - bat
      - eza
      - zoxide
      - git-delta
      - jq
    casks:
      - rio

  # Arch Linux — installed via paru (also resolves official-repo packages)
  linux:
    paru:
      - nushell
      - starship
      - rio
      - emacs
      - git
      - gnupg
      - ripgrep
      - fd
      - fzf
      - bat
      - eza
      - zoxide
      - git-delta
      - jq

  # Windows — winget
  windows:
    winget:
      - Nushell.Nushell
      - Starship.Starship
      - raphamorim.rio
      - GNU.Emacs
      - Git.Git
      - GnuPG.GnuPG
      - BurntSushi.ripgrep.MSVC
      - sharkdp.fd
      - junegunn.fzf
      - sharkdp.bat
      - eza-community.eza
      - ajeetdsouza.zoxide
      - dandavison.delta
      - jqlang.jq
```

- [ ] **Step 2: Verify it is valid YAML**

Run: `python3 -c "import yaml; yaml.safe_load(open('home/.chezmoidata/packages.yaml'))" || python3 -c "import sys; print('PyYAML missing — install or skip', file=sys.stderr)"`
Expected: exit 0 (install PyYAML with `brew install libyaml && pip3 install pyyaml` only if needed), or note skip.

- [ ] **Step 3: Verify chezmoi exposes the data to templates**

Run: `chezmoi --source "$PWD" execute-template '{{ .packages.darwin.casks | toJson }}'`
Expected: prints `["rio"]`.

Run: `chezmoi --source "$PWD" execute-template '{{ len .packages.linux.paru }}'`
Expected: prints `14`.

- [ ] **Step 4: Commit**

```bash
git add -A
git commit -m "feat: declare per-OS package lists in .chezmoidata/packages.yaml"
```

---

### Task 8: Unix package-install script (macOS + Arch)

Create the `run_onchange_before_` script that installs Homebrew formulae/casks on macOS and bootstraps `paru` + installs packages on Arch. It renders to empty on Windows. Running *before* file application guarantees `nu` exists when rio's template is evaluated.

**Files:**
- Create: `home/run_onchange_before_install-packages.sh.tmpl`

**Interfaces:**
- Consumes: `.packages.darwin.*`, `.packages.linux.paru`, `.chezmoi.os`.
- Produces: installed packages; `nu`, `starship`, `zoxide`, `rio`, etc. on PATH.

- [ ] **Step 1: Write `home/run_onchange_before_install-packages.sh.tmpl`**

Exact contents:

```bash
{{- if ne .chezmoi.os "windows" -}}
#!/usr/bin/env bash
set -euo pipefail

# chezmoi re-runs this whenever the rendered content (i.e. the package list) changes.

os="{{ .chezmoi.os }}"

if [ "$os" = "darwin" ]; then
    if ! command -v brew >/dev/null 2>&1; then
        echo "==> Installing Homebrew"
        NONINTERACTIVE=1 /bin/bash -c \
            "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
        if [ -x /opt/homebrew/bin/brew ]; then
            eval "$(/opt/homebrew/bin/brew shellenv)"
        elif [ -x /usr/local/bin/brew ]; then
            eval "$(/usr/local/bin/brew shellenv)"
        fi
    fi
    echo "==> Installing Homebrew formulae"
    brew install {{ range .packages.darwin.brews }}{{ . | quote }} {{ end }}
    echo "==> Installing Homebrew casks"
    brew install --cask {{ range .packages.darwin.casks }}{{ . | quote }} {{ end }}

elif [ "$os" = "linux" ]; then
    # Arch Linux only.
    if ! command -v paru >/dev/null 2>&1; then
        echo "==> Bootstrapping paru (AUR helper)"
        sudo pacman -S --needed --noconfirm base-devel git
        tmpdir="$(mktemp -d)"
        git clone https://aur.archlinux.org/paru-bin.git "$tmpdir/paru-bin"
        ( cd "$tmpdir/paru-bin" && makepkg -si --noconfirm )
        rm -rf "$tmpdir"
    fi
    echo "==> Installing packages via paru"
    paru -S --needed --noconfirm {{ range .packages.linux.paru }}{{ . | quote }} {{ end }}
fi
{{- end -}}
```

- [ ] **Step 2: Verify it renders to non-empty bash on the macOS dev box**

Run: `chezmoi --source "$PWD" execute-template < home/run_onchange_before_install-packages.sh.tmpl | tee /tmp/pkg.sh | head -5`
Expected: starts with `#!/usr/bin/env bash`; contains `brew install` lines including the formulae and `--cask "rio"`.

- [ ] **Step 3: Shellcheck the rendered script**

Run: `shellcheck -S warning /tmp/pkg.sh`
Expected: no errors (warnings about unreachable Arch branch are acceptable; resolve any genuine syntax errors).

- [ ] **Step 4: Verify the script body is bash-parseable**

Run: `bash -n /tmp/pkg.sh`
Expected: exit code 0 (parse-only; does NOT execute — do not run the script for real).

- [ ] **Step 5: Verify Windows renders empty (so chezmoi skips it there)**

Run: `chezmoi --source "$PWD" execute-template --init --promptString 'x=y' '{{ if ne "windows" "windows" }}body{{ end }}'` is not reliable; instead inspect the guard by confirming the file's first line is the `{{- if ne .chezmoi.os "windows" -}}` guard.

Run: `head -1 home/run_onchange_before_install-packages.sh.tmpl`
Expected: `{{- if ne .chezmoi.os "windows" -}}` (guarantees empty render on Windows → chezmoi skips empty scripts).

- [ ] **Step 6: Commit**

```bash
git add -A
git commit -m "feat: add unix package-install script (brew + paru), runs before apply"
```

---

### Task 9: Windows package-install script (winget + fonts + nushell junction)

Create the `run_onchange_before_` PowerShell script: install winget packages, download+register Aporetic fonts (no winget package exists), and junction `%APPDATA%\nushell` → `~/.config/nushell` so nushell finds its chezmoi-managed config. Renders empty on non-Windows.

**Files:**
- Create: `home/run_onchange_before_install-packages.ps1.tmpl`

**Interfaces:**
- Consumes: `.packages.windows.winget`, `.chezmoi.os`.
- Produces: installed Windows packages, registered fonts, nushell config junction.

- [ ] **Step 1: Write `home/run_onchange_before_install-packages.ps1.tmpl`**

Exact contents:

```powershell
{{- if eq .chezmoi.os "windows" -}}
# Managed by chezmoi. Re-runs when the rendered package list changes.
$ErrorActionPreference = "Stop"

$packages = @(
{{- range .packages.windows.winget }}
    "{{ . }}"
{{- end }}
)

foreach ($id in $packages) {
    Write-Host "==> Installing $id"
    winget install --exact --id $id `
        --accept-source-agreements --accept-package-agreements --silent
}

# --- Aporetic fonts (no winget package; install + register per-user) ---
$fontVersion = "1.2.0"
$fontUrl = "https://github.com/protesilaos/aporetic/archive/refs/tags/$fontVersion.tar.gz"
$work = Join-Path $env:TEMP "aporetic-$fontVersion"
New-Item -ItemType Directory -Path $work -Force | Out-Null
$tarball = Join-Path $work "aporetic.tar.gz"
Invoke-WebRequest -Uri $fontUrl -OutFile $tarball
tar -xzf $tarball -C $work
$fontDir = Join-Path $env:LOCALAPPDATA "Microsoft\Windows\Fonts"
New-Item -ItemType Directory -Path $fontDir -Force | Out-Null
Get-ChildItem -Path $work -Recurse -Filter "*.ttf" |
    Where-Object { $_.FullName -match "\\TTF\\" } |
    ForEach-Object {
        $dest = Join-Path $fontDir $_.Name
        Copy-Item $_.FullName $dest -Force
        $regName = [System.IO.Path]::GetFileNameWithoutExtension($_.Name) + " (TrueType)"
        New-ItemProperty -Path "HKCU:\Software\Microsoft\Windows NT\CurrentVersion\Fonts" `
            -Name $regName -Value $dest -PropertyType String -Force | Out-Null
    }

# --- Make nushell read ~/.config/nushell (its Windows default is %APPDATA%\nushell) ---
$nuTarget = Join-Path $HOME ".config\nushell"
$nuDefault = Join-Path $env:APPDATA "nushell"
if ((Test-Path $nuTarget) -and -not (Test-Path $nuDefault)) {
    New-Item -ItemType Junction -Path $nuDefault -Target $nuTarget | Out-Null
}
{{- end -}}
```

- [ ] **Step 2: Verify it renders empty on the macOS dev box (so it never runs on Unix)**

Run: `chezmoi --source "$PWD" execute-template < home/run_onchange_before_install-packages.ps1.tmpl | tr -d '[:space:]' | wc -c`
Expected: `0` (empty after whitespace strip → chezmoi skips it on macOS/Linux).

- [ ] **Step 3: Verify the guard line**

Run: `head -1 home/run_onchange_before_install-packages.ps1.tmpl`
Expected: `{{- if eq .chezmoi.os "windows" -}}`.

- [ ] **Step 4: Static PowerShell syntax check if pwsh is available (optional)**

Run: `command -v pwsh >/dev/null && chezmoi --source "$PWD" execute-template '{{- if eq "windows" "windows" -}}{{ range .packages.windows.winget }}{{ . }}{{ end }}{{- end -}}' | grep -q raphamorim.rio && echo OK || echo "skipped/ok"`
Expected: `OK` or `skipped/ok` (confirms the winget list includes rio when the Windows branch is forced).

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat: add windows package-install script (winget + fonts + nushell junction)"
```

---

### Task 10: chezmoi externals (emacs.d + Aporetic fonts)

Add `.chezmoiexternal.toml` to clone the emacs config repo into `~/.config/emacs`, and (on macOS/Linux) extract Aporetic `.ttf` files into the platform font dir. Windows fonts are handled by Task 9.

**Files:**
- Create: `home/.chezmoiexternal.toml`

**Interfaces:**
- Consumes: `.chezmoi.os`.
- Produces: `~/.config/emacs` (git clone) and, on macOS/Linux, Aporetic fonts under the platform font dir.

- [ ] **Step 1: Write `home/.chezmoiexternal.toml`**

Exact contents (this file is always treated as a template by chezmoi):

```toml
[".config/emacs"]
    type = "git-repo"
    url = "https://github.com/felipebalbi/emacs.d.git"
    refreshPeriod = "168h"

{{- if ne .chezmoi.os "windows" }}
{{- $fontDir := ".local/share/fonts/aporetic" }}
{{- if eq .chezmoi.os "darwin" }}{{ $fontDir = "Library/Fonts" }}{{ end }}

["{{ $fontDir }}"]
    type = "archive"
    url = "https://github.com/protesilaos/aporetic/archive/refs/tags/1.2.0.tar.gz"
    stripComponents = 3
    include = ["**/TTF/*.ttf"]
    refreshPeriod = "672h"
{{- end }}
```

- [ ] **Step 2: Verify it renders to valid TOML on macOS (emacs entry + Library/Fonts entry)**

Run: `chezmoi --source "$PWD" execute-template < home/.chezmoiexternal.toml | tee /tmp/ext.toml`
Expected: contains `[".config/emacs"]` with the emacs.d URL, and `["Library/Fonts"]` with the aporetic tag tarball URL, `stripComponents = 3`, `include = ["**/TTF/*.ttf"]`.

- [ ] **Step 3: Validate rendered TOML parses**

Run: `python3 -c "import sys,tomllib; tomllib.load(open('/tmp/ext.toml','rb')); print('ok')"`
Expected: prints `ok`.

- [ ] **Step 4: Verify chezmoi lists the externals as managed**

Run: `chezmoi --source "$PWD" managed | grep -E 'config/emacs|fonts|Fonts'`
Expected: shows `.config/emacs` and the font directory among managed entries.

- [ ] **Step 5: (Optional, network) Verify the font external extracts the expected flat ttf set into a temp destination**

```bash
DEST="$(mktemp -d)"
chezmoi --source "$PWD" --destination "$DEST" apply --dry-run --verbose --refresh-externals 2>&1 | grep -i aporetic | head
```

Run the block above.
Expected: dry-run output references writing `Library/Fonts/...AporeticSerifMono...ttf`-style flat files (filenames vary). If offline, note "skipped — no network" and rely on Steps 2–4. Do NOT drop `--dry-run`.

- [ ] **Step 6: Commit**

```bash
git add -A
git commit -m "feat: add externals for emacs.d clone and Aporetic fonts"
```

---

### Task 11: defaults script (login shell + default terminal + font cache)

Create the `run_once_after_` script: set nushell as the login shell (macOS/Arch), set rio as the default terminal on Arch, and refresh the Linux font cache. Renders empty on Windows.

**Files:**
- Create: `home/run_once_after_set-defaults.sh.tmpl`

**Interfaces:**
- Consumes: `.chezmoi.os`, `nu` on PATH (from Task 8).
- Produces: login shell = nushell; `xdg-settings` default terminal = rio (Arch); refreshed font cache.

- [ ] **Step 1: Write `home/run_once_after_set-defaults.sh.tmpl`**

Exact contents:

```bash
{{- if ne .chezmoi.os "windows" -}}
#!/usr/bin/env bash
set -euo pipefail

# --- Set nushell as the default login shell ---
nu_path="$(command -v nu || true)"
if [ -n "$nu_path" ]; then
    if ! grep -qxF "$nu_path" /etc/shells 2>/dev/null; then
        echo "$nu_path" | sudo tee -a /etc/shells >/dev/null
    fi
    if [ "${SHELL:-}" != "$nu_path" ]; then
        chsh -s "$nu_path" || echo "chsh failed; set your login shell to $nu_path manually."
    fi
fi

{{ if eq .chezmoi.os "linux" -}}
# --- Arch Linux: rio as the default terminal + refresh font cache ---
if command -v xdg-settings >/dev/null 2>&1; then
    xdg-settings set default-terminal-emulator rio.desktop 2>/dev/null || true
fi
if command -v fc-cache >/dev/null 2>&1; then
    fc-cache -f >/dev/null 2>&1 || true
fi
{{- end }}
{{- end -}}
```

- [ ] **Step 2: Verify it renders non-empty bash on macOS, empty body for the linux-only block**

Run: `chezmoi --source "$PWD" execute-template < home/run_once_after_set-defaults.sh.tmpl | tee /tmp/defaults.sh`
Expected: starts with `#!/usr/bin/env bash`, contains the `chsh -s` block; on a macOS render the `xdg-settings`/`fc-cache` block is absent.

- [ ] **Step 3: Shellcheck + parse-check (do not execute)**

Run: `shellcheck -S warning /tmp/defaults.sh && bash -n /tmp/defaults.sh`
Expected: no errors; exit 0. (Never run this script for real — it changes your login shell.)

- [ ] **Step 4: Verify the guard line**

Run: `head -1 home/run_once_after_set-defaults.sh.tmpl`
Expected: `{{- if ne .chezmoi.os "windows" -}}`.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat: add run_once defaults (nushell login shell, rio default terminal, fc-cache)"
```

---

### Task 12: root bootstrap wrappers + README

Add memorable `install.sh` / `install.ps1` wrappers around the chezmoi bootstrap, and rewrite the README with the one-liners, structure, and usage.

**Files:**
- Create: `install.sh`
- Create: `install.ps1`
- Modify: `README.md` (full rewrite)

**Interfaces:**
- Produces: a fresh-machine entry point. `install.sh` and the README one-liner both end at `chezmoi init --apply <repo URL>`.

- [ ] **Step 1: Write `install.sh`**

Exact contents:

```sh
#!/usr/bin/env sh
# Bootstrap dotconfig on macOS or Arch Linux:
# install chezmoi, clone this repo, and apply it.
set -eu

REPO="https://github.com/felipebalbi/dotconfig.git"

# Installs chezmoi to a temp dir and runs `chezmoi init --apply <REPO>`.
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply "$REPO"
```

- [ ] **Step 2: Shellcheck and parse-check install.sh**

Run: `shellcheck install.sh && sh -n install.sh`
Expected: no findings; exit 0.

- [ ] **Step 3: Write `install.ps1`**

Exact contents:

```powershell
# Bootstrap dotconfig on Windows: install chezmoi via winget, then init+apply.
$ErrorActionPreference = "Stop"

$Repo = "https://github.com/felipebalbi/dotconfig.git"

if (-not (Get-Command chezmoi -ErrorAction SilentlyContinue)) {
    winget install --exact --id twpayne.chezmoi `
        --accept-source-agreements --accept-package-agreements
    # Refresh PATH for the current session so `chezmoi` is found.
    $env:Path = [System.Environment]::GetEnvironmentVariable("Path", "Machine") + ";" +
                [System.Environment]::GetEnvironmentVariable("Path", "User")
}

chezmoi init --apply $Repo
```

- [ ] **Step 4: Rewrite `README.md`**

Exact contents:

````markdown
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
  dot_config/         # -> ~/.config/* (rio, nushell, starship)
  dot_gitconfig       # -> ~/.gitconfig
  .chezmoidata/       # packages.yaml: per-OS package lists
  .chezmoiexternal.toml         # emacs.d clone + Aporetic fonts
  run_onchange_before_install-packages.sh.tmpl   # macOS/Arch package install
  run_onchange_before_install-packages.ps1.tmpl  # Windows package install
  run_once_after_set-defaults.sh.tmpl            # login shell + default terminal
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
- **Fonts:** Aporetic is not a Nerd Font, so prompt/CLI configs avoid glyph
  icons.
- Old window-manager and system configs live under `legacy/` and are not touched
  by chezmoi.
````

- [ ] **Step 5: Verify the README one-liner matches the wrapper and spec**

Run: `grep -c "felipebalbi/dotconfig.git" README.md install.sh install.ps1`
Expected: each file reports at least 1 match (consistent repo URL across all three).

- [ ] **Step 6: Commit**

```bash
git add -A
git commit -m "feat: add bootstrap wrappers (install.sh/.ps1) and rewrite README"
```

---

### Task 13: end-to-end dry-run verification

Prove the whole source tree maps to the correct destination paths in one shot, against a throwaway destination — without mutating the real machine.

**Files:** none (verification only).

- [ ] **Step 1: chezmoi doctor**

Run: `chezmoi --source "$PWD" doctor`
Expected: no `error` lines for the features we use (chezmoi found, git found). `warning`s for unused integrations are fine.

- [ ] **Step 2: Full managed-entry listing**

Run: `chezmoi --source "$PWD" managed`
Expected includes: `.config/nushell/config.nu`, `.config/nushell/env.nu`, `.config/rio/config.toml`, `.config/rio/themes/catppuccin-mocha.toml`, `.config/starship.toml`, `.gitconfig`, `.config/emacs`, and the font dir.

- [ ] **Step 3: Dry-run apply to a temp destination (no real-home writes, scripts not executed for real)**

```bash
DEST="$(mktemp -d)"
chezmoi --source "$PWD" --destination "$DEST" apply --dry-run --verbose
echo "exit: $?"
```

Run the block above.
Expected: `exit: 0`; verbose output shows it would create `.gitconfig`, `.config/rio/...`, `.config/nushell/...`, `.config/starship.toml`, and would run the `run_onchange_before` (unix) + `run_once_after` scripts. No template errors.

- [ ] **Step 4: Confirm the Windows-only script is skipped on the dev OS**

Run: `chezmoi --source "$PWD" --destination "$(mktemp -d)" apply --dry-run --verbose 2>&1 | grep -i 'install-packages.ps1' || echo "ps1 correctly skipped"`
Expected: prints `ps1 correctly skipped` (the PowerShell script renders empty on macOS and is not scheduled).

- [ ] **Step 5: Document the manual fresh-machine test in the plan's wake**

Confirm (by reading, not running) that the README one-liners are the intended real-machine test:
- macOS/Arch: the `get.chezmoi.io` one-liner.
- Windows: the winget + `chezmoi init --apply` flow.

No commit needed unless Steps 1–4 surfaced fixes; if they did, commit them with `fix:` messages.

---

## Self-Review

**Spec coverage:**
- chezmoi engine + managed-files mode → Tasks 2–13. ✓
- `home/` via `.chezmoiroot`, `legacy/` preservation → Tasks 1–2. ✓
- Migration map (rio, gitconfig, nushell, starship, emacs external) → Tasks 3, 4, 5, 6, 10. ✓
- Bootstrap one-liners (macOS/Arch + Windows) with explicit repo URL → Task 12. ✓
- Packages via `run_onchange_` + `packages.yaml` (brew/paru/winget) → Tasks 7, 8, 9. ✓
- Aporetic fonts via tag tarball (no releases), glyph-free configs → Tasks 6, 9, 10. ✓
- rio default terminal everywhere ($TERMINAL + xdg, documented macOS/Windows limits) → Tasks 5, 11, 12. ✓
- nushell-on-Windows path handling (junction) → Task 9. ✓
- Default login shell via run_once → Task 11. ✓
- Templating wins (rio nu path, OS-guarded scripts) → Tasks 4, 8, 9, 10, 11. ✓

**Placeholder scan:** No TBD/TODO; every file has complete contents; every step has an exact command + expected result.

**Type/name consistency:** Data keys `.packages.darwin.brews|casks`, `.packages.linux.paru`, `.packages.windows.winget` are defined in Task 7 and consumed verbatim in Tasks 8–9. Script names use chezmoi's `run_onchange_before_` / `run_once_after_` ordering tokens consistently. Repo URL `https://github.com/felipebalbi/dotconfig.git` identical in Tasks 12 and the spec. Font tag `1.2.0` identical in Tasks 9 and 10.

## Execution Handoff

(To be offered after the user reviews this plan.)
