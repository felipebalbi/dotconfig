#!/usr/bin/env sh
# Bootstrap dotconfig on macOS or Arch Linux:
# install chezmoi, clone this repo, and apply it.
set -eu

REPO="https://github.com/felipebalbi/dotconfig.git"

# Installs chezmoi to a temp dir and runs `chezmoi init --apply <REPO>`.
sh -c "$(curl -fsLS get.chezmoi.io)" -- init --apply "$REPO"
