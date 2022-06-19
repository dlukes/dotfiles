#!/bin/sh

# Sets ID, as in distro ID, and other distro-info-related variables.
os_release=/etc/os-release
[ -r "$os_release" ] && . "$os_release"

is_fedora() {
  [ "$ID" = fedora ]
}

is_ubuntu() {
  [ "$ID" = ubuntu ]
}

is_macos() {
  [ "$( uname )" = Darwin ]
}

am_admin() {
  # sudo -v is normally used to extend the sudo password timeout, but just exits 1 if
  # the user doesn't have any sudo privileges.
  sudo -v
}

brew_install_or_upgrade() {
  package="$1"
  upgrade='brew upgrade'
  install='brew install'
  case "${2:-}" in
    "")
      ;;
    --head)
      upgrade="$upgrade --fetch-HEAD"
      install="$install --fetch-HEAD --HEAD"
      ;;
    *)
      >&2 echo "brew_install_or_upgrade: invalid arg $2"
      exit 1
      ;;
  esac
  if brew ls --versions "$package" >/dev/null; then
    $upgrade "$package"
  else
    $install "$package"
  fi
}

# Fetch installation archive if cmd is not available.
maybe_fetch_archive() {
  local cmd="$1"
  local repo="$2"
  local archive_regex="$3"
  if ! command -v "$cmd" >/dev/null; then
    release_link=https://github.com$(
      curl -sfL "https://github.com/$repo/releases" |
        grep -oPm1 '/[^"]+/'"$archive_regex"
    )
    curl -sSfLO "$release_link"
    basename "$release_link"
  else
    >&2 echo ">>> $(command -v "$cmd") is already available, skipping installation."
  fi
}

# On macOS, GNU coreutils et al. overriding native tooling might have
# adverse side effects (e.g. Python fails to compile on Apple Silicon).
# This provides a way to run a command in an environment where they're
# not accessible.
without_gnubin() {
  full_path="$PATH"
  export PATH=$(echo "$full_path" | sed 's/[^:]\+\/gnubin://g')
  "$@"
  export PATH="$full_path"
}

github_latest_release_tag_name() {
  local org="$1"; shift
  local repo="$1"; shift
  # Read full response into variable to avoid error exit code 23 from curl (failed
  # writing body).
  local response=$(
    curl -sSfL -H "Accept: application/vnd.github.v3+json" \
      https://api.github.com/repos/"$org"/"$repo"/releases/latest
  )
  echo "$response" |
    grep -oPm1 '"tag_name":\s*"[^"]+?"' |
    cut -d\" -f4
}

# Invoke curl; if that fails because of an SSL certificate error, retry with curl -k.
curlk() {
  curl "$@" 2>/dev/null || curl -k "$@"
}

should_update() {
  local cmd="$1"; shift
  local username="$1"; shift
  local repo="$1"; shift

  if [ -d "$repo" ]; then
    (
      cd "$repo"
      git fetch --quiet
      # @{u} is the current branch's upstream
      if [ $(git rev-parse HEAD) = $(git rev-parse @{u}) ] && command -v $cmd >/dev/null; then
        >&2 echo ">>> $username/$repo: newest version is already installed, aborting."
        return 1
      fi
      git pull
    )
  else
    git clone --depth 1 https://github.com/"$username"/"$repo".git
  fi
}
