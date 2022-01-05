#!/bin/sh

set -euf
script_dir=$(dirname "$(realpath "$0")")
. "$script_dir"/util.sh

export NPM_CONFIG_PREFIX="$HOME/.local"
alias curl='curl --silent'
if is_macos; then
  os=mac
  alias grep=ggrep
else
  os=linux
fi

cd "$NPM_CONFIG_PREFIX/bin"

>&2 echo 'Installing elm...'
target=elm.gz
curl -L -o $target https://github.com/elm/compiler/releases/latest/download/binary-for-$os-64-bit.gz
gunzip -f $target
chmod +x elm
rm -f $target

>&2 echo 'Installing elm-format...'
elm_format_version=$(
  curl https://github.com/avh4/elm-format/releases/ |
    grep /avh4/elm-format/releases/tag |
    head -n 1 |
    grep -oP '>.*?<' |
    tr -d '><'
)
target=elm-format.tgz
curl -L -o $target https://github.com/avh4/elm-format/releases/download/$elm_format_version/elm-format-$elm_format_version-$os-x64.tgz
tar xzf $target
rm -f $target

>&2 echo 'Installing elm-language-server and elm-test...'
npm install -g --ignore-scripts @elm-tooling/elm-language-server elm-test
