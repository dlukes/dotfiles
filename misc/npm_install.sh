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

pkgs='pyright bash-language-server vim-language-server typescript typescript-language-server prettier'
npm install -g --ignore-scripts $pkgs
npm update  -g --ignore-scripts $pkgs

# vi: foldmethod=marker
