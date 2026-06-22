# Managed by chezmoi.

# rio is the default terminal everywhere.
$env.TERMINAL = "rio"

{{ if eq .chezmoi.os "darwin" -}}
# macOS GUI/login shells often lack Homebrew's bin dirs; ensure brew-installed
# tools (starship, zoxide, eza, ...) resolve in nushell sessions.
$env.PATH = ($env.PATH | prepend ["/opt/homebrew/bin" "/opt/homebrew/sbin" "/usr/local/bin"] | uniq)

{{ end -}}
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
