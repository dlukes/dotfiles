#!/bin/sh

set -euf
script_dir=$(dirname "$(realpath "$0")")
prefix="$HOME"/.local/mambaforge
installer="Mambaforge-$(uname)-$(uname -m).sh"
if [ -d "$prefix" ]; then
  opts=-bup
else
  opts=-bp
fi

curl -sSfLO https://github.com/conda-forge/miniforge/releases/latest/download/"$installer"
bash "$installer" $opts "$prefix"
rm "$installer"
"$prefix"/bin/mamba env update --name base --file "$script_dir"/base.yml
conda activate base
"$script_dir"/../bin/pdm_install.sh
