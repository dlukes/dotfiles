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
mamba env update --name umrk --file "$script_dir"/umrk.yml
. "$prefix"/etc/profile.d/conda.sh
conda activate umrk

>&2 echo ">>> Running additional Pip installs with $(command -v python3)."
"$script_dir"/../python/umrk/install.sh

>&2 echo ">>> Running additional NPM installs with $(command -v npm)."
"$script_dir"/../misc/npm_install.sh

>&2 echo '>>> Default environment is umrk. Remember that environments can be stacked.'
