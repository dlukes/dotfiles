#!/bin/sh

set -euf

if python -c "
import sys
from pathlib import Path
for dir in sys.path:
  if (Path(dir) / 'umrk.egg-link').is_file():
    sys.exit(0)
sys.exit(1)
"; then
  >&2 echo "UMRK is already installed in editable mode."
  exit
fi

>&2 echo "Installing UMRK in editable mode."
script_dir=$(dirname "$(realpath "$0")")
cd "$script_dir"

# generate a setup.py
poetry build
tar xzOf dist/umrk-0.1.0.tar.gz --no-anchored setup.py >setup.py
rm -rf dist

# temporarily hide the pyproject.toml; required because of some bug in setuptools or
# pip, otherwise the editable install falls over
mv pyproject.toml pyproject.toml:disabled
pip install -e .
mv pyproject.toml:disabled pyproject.toml
rm setup.py
