#!/bin/sh

is_macos() {
  [ "$( uname )" == Darwin ]
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
