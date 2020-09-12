#!/bin/sh

cd "$( mktemp -d )"
git clone https://github.com/junegunn/fzf
cd fzf
./install --bin
for bin in bin/*; do
  mv -ft "$HOME/.local/bin" "$bin"
done
