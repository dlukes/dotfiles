#!/bin/sh

set -eufx
script_dir=$(dirname "$0")
prefix="$HOME"/.local
. util.sh

>&2 echo '>>> Installing zstd.'
cd "$(mktemp -d)"
archive=$(maybe_fetch_archive zsitd facebook/zstd 'zstd-[\d.]+.tar.gz')
if [ -n "$archive" ]; then
  tar xzf "$archive"
  cd "${archive%.tar.gz}"
  make install PREFIX="$prefix"
  >&2 echo '>>> Installed zstd.'
else
  >&2 echo '>>> zstd already installed.'
fi

