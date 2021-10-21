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
if [ -d "$prefix"/$repo ] && false; then
  >&2 echo ">>> Detected git repo, compiling from source."
  cd "$prefix"
  if should_update $cmd $org $repo; then
    >&2 echo ">>> Compiling Neovim from source..."
    cd $repo
    make -j4 CMAKE_BUILD_TYPE=Release CMAKE_INSTALL_PREFIX="$prefix"
    make install
    >&2 echo ">>> Installed $(nvim --version | head -1)."
  else
    >&2 echo ">>> Neovim is up to date at $(nvim --version | head -1)."
  fi
else
  >&2 echo ">>> No git repo, downloading appimage."
  >&2 echo "

Automation of this step is currently blocked on <https://github.com/neovim/neovim/issues/15709>,
nightlies can't reliably be found at <https://github.com/neovim/neovim/releases/nightly>.

Please go to <https://github.com/neovim/neovim/actions/workflows/release.yml>, click on
the latest release (even if it's failed), scroll down to Artifacts and download the
appimage manually. Then press ENTER here to continue.

"
  read __ignored_reply
  # Something like appimage=$(maybe_fetch_archive nvimm $org/$repo nvim.appimage) etc.
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
