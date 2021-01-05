#!/bin/sh

set -euf



# ---------------------------------------------------------- Node itself {{{1


>&2 echo '>>> Installing Node...'

install_node() {
  prefix="$HOME"/.local
  path=$(command -v node)
  # check if a separate installation of Node exists somewhere outside
  # the target prefix; if so, maybe we don't want to install Node after
  # all
  case "$path" in
    "$prefix"*) ;;
    *)
      >&2 echo ">>> Detected Node outside target prefix, as $path."
      read -ep ">>> Install Node under $prefix alongside this existing installation anyway? (y/n) " yn
      case "$yn" in
        Y*|y*) ;;
        *) return ;;
      esac
      ;;
  esac

  version=lts
  installed=$(node --version 2>/dev/null || echo none)
  requested=$(curl -sL https://resolve-node.now.sh/$version)
  if [ "$installed" != "$requested" ]; then
    curl -sSfL install-node.now.sh/$version | bash -s -- --prefix="$prefix"
  else
    >&2 echo "Most recent $version version already installed: $installed."
  fi
}

install_node



# --------------------------------------------------------- NPM packages {{{1


>&2 echo '>>> Installing NPM packages...'

install_or_update() {
  if command -v "$1" >/dev/null; then
    cmd=update
  else
    cmd=install
  fi
  npm $cmd -g "$1"
}

install_or_update npm
install_or_update pyright
install_or_update bash-language-server
install_or_update @elm-tooling/elm-language-server
install_or_update vim-language-server
install_or_update typescript
install_or_update typescript-language-server
