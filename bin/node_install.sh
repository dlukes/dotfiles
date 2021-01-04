#!/bin/sh

set -euf

install_or_update() {
  if command -v "$1" >/dev/null; then
    cmd=update
  else
    cmd=install
  fi
  npm $cmd -g "$1"
}

version=lts
installed=$(node --version 2>/dev/null || echo none)
requested=$(curl -sL https://resolve-node.now.sh/$version)
if [ "$installed" != "$requested" ]; then
  curl -sSfL install-node.now.sh/$version | bash -s -- --prefix="$HOME/.local"
fi

install_or_update npm
install_or_update pyright
install_or_update bash-language-server
install_or_update @elm-tooling/elm-language-server
install_or_update vim-language-server
install_or_update typescript
install_or_update typescript-language-server
