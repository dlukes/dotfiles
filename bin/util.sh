#!/bin/sh

is_macos() {
  [ "$( uname )" = Darwin ]
}

brew_install_or_upgrade() {
  package="$1"
  if brew ls --versions "$package" >/dev/null; then
    brew upgrade "$package"
  else
    brew install "$package"
  fi
}

# Fetch installation archive if cmd is not available, else return 10.
maybe_fetch_archive() {
  local cmd="$1"
  local repo="$2"
  local archive_regex="$3"
  if ! command -v "$cmd" >/dev/null || [ -n "$GOFISH_FORCE" ]; then
    release_link=https://github.com$(
      curl -sL "https://github.com/$repo/releases" |
        grep -oPm1 '/[^"]+/'"$archive_regex"
    )
    curl -sLO "$release_link"
    basename "$release_link"
  else
    return 10
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
  curl -sSfL -H "Accept: application/vnd.github.v3+json" \
    https://api.github.com/repos/"$org"/"$repo"/releases/latest |
    grep -oPm1 '"tag_name":\s*"[^"]+?"' |
    cut -d\" -f4
}

# Invoke curl; if that fails because of an SSL certificate error, retry with curl -k.
curlk() {
  curl "$@" 2>/dev/null || curl -k "$@"
}
