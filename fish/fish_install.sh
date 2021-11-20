#!/bin/sh

set -eu

mandir="$HOME/.local/share/man/man1"
bindir="$HOME/.local/bin"
workdir=$(mktemp -d)
cd "$workdir"

# Just installs fasd for the moment!

fasd_release_link=https://github.com$(
  curl -sL https://github.com/clvv/fasd/tags |
    grep -oPm1 '/[^"]+\.tar\.gz'
)
curl -sLO "$fasd_release_link"
tar xzf $(basename "$fasd_release_link")
cp fasd-*/fasd.1 "$mandir"
cp fasd-*/fasd "$bindir"
