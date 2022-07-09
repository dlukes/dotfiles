#!/bin/sh

set -euf
script_dir=$(dirname "$(realpath "$0")")

if command -v broot >/dev/null 2>&1; then
  cargo install-update broot
else
  cargo install --locked broot
fi

broot --write-default-conf "$script_dir"
broot --print-shell-function fish |
  sed -e "s| broot \(.*argv\)| broot --conf '$script_dir/custom.hjson' \1|" \
  >"$script_dir"/../fish/functions/br.fish
