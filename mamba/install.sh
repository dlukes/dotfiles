#!/bin/sh

set -euf
script_dir=$(dirname "$(realpath "$0")")
. "$script_dir"/../misc/util.sh

prefix="$HOME"/.local/mambaforge
conda_config="$prefix"/etc/profile.d/conda.sh
installer="Mambaforge-$(uname)-$(uname -m).sh"
default_env=umrk
env_file="$script_dir"/$default_env.yml

info 'Unsetting Conda-related environment variables, if any.'
for var in $(env | grep CONDA); do
  var="${var%=*}"
  >&2 echo "Unsetting $var."
  unset "$var"
done

info "Downloading and installing $installer."
curl -sSfLO https://github.com/conda-forge/miniforge/releases/latest/download/"$installer"
bash "$installer" -bup "$prefix"
rm "$installer"

info "Setting up and activating default environment $default_env from $env_file."
. "$conda_config"
mamba env update --name $default_env --file "$env_file"
conda activate $default_env

info "Installing UMRK Python package for $(command -v python3)."
"$script_dir"/../python/umrk/install.sh

info "Running additional NPM installs with $(command -v npm)."
"$script_dir"/../misc/npm_install.sh

# TODO: This is very hamfisted and potentially breaks things. conda-tree tells me only
# matplotlib needs libtool via PyQT, which I'm unlikely to ever need, but who knows.
find "$prefix/envs/$default_env/bin" \( -name libtool -or -name libtoolize \) -delete

info "Default environment is $default_env. Remember that environments can be stacked."
