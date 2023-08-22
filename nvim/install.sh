#!/bin/sh

set -euf
script_dir=$(dirname "$(realpath "$0")")
. "$script_dir/../misc/util.sh"

prefix="$HOME/.local"
org=neovim
repo=$org
cmd=nvim

>&2 echo ">>> Installing Neovim..."
if is_macos; then
  brew_install_or_upgrade nvim --head
elif [ -d "$prefix"/$repo ]; then
  >&2 echo ">>> Detected git repo, will compile from source."
  "$script_dir"/../misc/ninja_install.sh
  "$script_dir"/../misc/cmake_install.sh
  cd "$prefix"
  if should_update $cmd $org $repo; then
    >&2 echo ">>> Compiling Neovim from source..."
    cd $repo
    make distclean
    make -j4 CMAKE_BUILD_TYPE=Release CMAKE_INSTALL_PREFIX="$prefix"
    make install
    >&2 echo ">>> Installed $(nvim --version | head -1)."
  else
    >&2 echo ">>> Neovim is up to date at $(nvim --version | head -1)."
  fi
else
  >&2 echo ">>> No git repo, downloading appimage."
  cd "$prefix"/bin
  appimage=nvim.appimage
  rm -f $appimage
  maybe_fetch_archive dummy-cmd-to-force-install $org/$repo nightly/$appimage
  chmod +x "$appimage"
  ln -sf "$appimage" nvim
fi

PACKER_SYNC=1 nvim +':set termguicolors'
