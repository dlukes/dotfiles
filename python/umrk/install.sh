#!/bin/sh

set -euf

if python -c "
import sys
from pathlib import Path
for dir in sys.path:
  p = Path(dir) / 'umrk.egg-link'
  if p.is_file():
    print(f'Found egg-link: {p}', file=sys.stderr)
    sys.exit(0)
sys.exit(1)
"; then
  >&2 echo "UMRK is already installed in editable mode."
  exit
fi

>&2 echo "Installing UMRK in editable mode."
script_dir=$(dirname "$(realpath "$0")")
cd "$script_dir"
python3 -m pip install -e .
