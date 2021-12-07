#!/bin/sh

set -euf

err=
if ! command -v mamba >/dev/null; then
  >&2 echo 'ERROR: Install Mambaforge first.'
  err=1
fi
if ! command -v npm >/dev/null; then
  >&2 echo 'ERROR: Install NodeJS with Mamba first.'
  err=1
fi
if [ -n "$err" ]; then
  exit 1
fi

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

# vi: foldmethod=marker
