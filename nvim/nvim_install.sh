#!/bin/sh

set -euf
script_dir=$(dirname "$0")
. "$script_dir/util.sh"

prefix="$HOME/.local"
org=neovim
repo=$org
cmd=nvim



# ------------------------------------------------ Install cmake and ninja build systems {{{1


"$script_dir"/ninja_install.sh
"$script_dir"/cmake_install.sh



# ----------------------------------------------------------------------- Install Neovim {{{1


>&2 echo ">>> Installing Neovim..."
if [ -d "$prefix"/$repo ]; then
  >&2 echo ">>> Detected git repo, compiling from source."
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
  appimage=$(maybe_fetch_archive dummy-cmd-to-force-nvim-to-always-install $org/$repo nvim.appimage)
  if [ -n "$appimage" ]; then
    chmod +x "$appimage"
    ln -sf "$appimage" nvim
  fi
fi



# --------------------------------------------------------------- Install/upgrade pynvim {{{1


pip3 install --upgrade --upgrade-strategy eager pynvim



# --------------------------------------------------------- Install vim-plug and plugins {{{1


plug_vim="$HOME/.config/nvim/autoload/plug.vim"
if [ ! -f "$plug_vim" ]; then
  mkdir -p "$(dirname "$plug_vim")"
  curl -fLo "$plug_vim" \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

# use only head of init.vim file containing plugin declarations relevant
# for PlugInstall -- until the plugins are available, the rest might
# cause errors which will abort the installation process
sed '/call plug#end()/q' "$script_dir/../nvim/init.vim" |
  nvim -u /dev/stdin +'PlugInstall --sync' +qall
nvim +UpdateEverything
