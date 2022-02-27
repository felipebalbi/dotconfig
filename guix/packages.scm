(use-modules (gnu))

(use-package-modules
 curl
 fonts
 gnupg
 gnuzilla
 guile-xyz
 linux
 password-utils
 pulseaudio
 python
 python-xyz
 rust
 rust-apps
 terminals
 video
 web-browsers
 xorg)

(packages->manifest
 (list
  curl
  ffmpeg
  gnupg
  guile-hall
  icecat
  mpv
  nyxt
  obs
  pamixer
  password-store
  picocom
  pinentry-emacs
  pinentry-tty
  poetry
  powertop
  python
  python-black
  python-lsp-server
  rust
  (list rust "cargo")
  (list rust "rustfmt")
  rust-analyzer
  xinput
  xrandr
  xsetroot
  ))
