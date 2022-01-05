#!/bin/sh

set -euf
script_dir=$(dirname "$(realpath "$0")")
prefix="$HOME"/.local/mambaforge
installer="Mambaforge-$(uname)-$(uname -m).sh"

curl -sSfLO https://github.com/conda-forge/miniforge/releases/latest/download/"$installer"
(
  unset CONDA_PREFIX PYTHONPATH
  bash "$installer" -bup "$prefix"
)
rm "$installer"

export PATH="$prefix/bin:$PATH"
mamba env update --name base --file "$script_dir"/base.yml
"$script_dir"/../python/pdm_install.sh
"$script_dir"/../python/umrk/install.sh
