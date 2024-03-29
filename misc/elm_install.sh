#!/bin/sh

set -euf
script_dir=$(dirname "$(realpath "$0")")
. "$script_dir"/util.sh

alias curl='curl --silent'
if is_macos; then
  os=mac
  alias grep=ggrep
else
  os=linux
fi

cd "$HOME"/.local/bin
>&2 echo '>>> Installing Elm...'
target=elm.gz
curl -L -o $target https://github.com/elm/compiler/releases/latest/download/binary-for-$os-64-bit.gz
gunzip -f $target
chmod +x elm
rm -f $target

>&2 echo '>>> Installing additional tooling from NPM...'
pkgs='@elm-tooling/elm-language-server elm-format elm-test'
npm install -g --ignore-scripts $pkgs
npm update  -g --ignore-scripts $pkgs

# TODO: More Elm tooling to consider using -- though note that as of 2022, these are
# still considered experimental and probably not needed for vanilla uses of Elm:
#
# https://github.com/mdgriffith/elm-optimize-level-2
# https://github.com/zwilias/elm-json
