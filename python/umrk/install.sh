#!/bin/sh

set -euf

if python3 -c 'import umrk' >/dev/null 2>&1; then
  >&2 echo "UMRK is already installed."
  exit
fi

script_dir=$(dirname "$(realpath "$0")")
cd "$script_dir"
python3 -m pip install -e .
