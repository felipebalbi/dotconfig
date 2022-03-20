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
 version-control
 video
 web-browsers
 xorg)

(packages->manifest
 (list
  curl
  ffmpeg
  git
  (list git "send-email")
  (list git "credential-netrc")
  (list git "credential-libsecret")
  (list git "subtree")
  gnupg
  guile-hall
  icecat
  mpv
  nyxt
  obs
  pamixer
  pass-otp
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
