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
