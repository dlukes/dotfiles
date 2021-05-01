#!/bin/sh

set -eu

scheme_dir="$HOME/.config/tilix/schemes"
tmp_dir=$(mktemp -d)

mkdir -p "$scheme_dir"
cd "$tmp_dir"
git clone --depth 1 https://gitlab.com/protesilaos/tempus-themes-tilix.git
mv -t "$scheme_dir" tempus-themes-tilix/*.json
