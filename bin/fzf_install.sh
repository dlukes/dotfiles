#!/bin/sh

cd "$( mktemp -d )"
git clone https://github.com/junegunn/fzf
cd fzf

./install --bin
for bin in bin/*; do
  mv -ft "$HOME/.local/bin" "$bin"
done

share="$HOME/.local/share/fzf"
mkdir -p "$share"
for shell in shell/*; do
  mv -t "$share" "$shell"
done
