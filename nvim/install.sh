#!/bin/sh

set -euf
script_dir=$(dirname "$(realpath "$0")")
. "$script_dir/../misc/util.sh"

prefix="$HOME/.local"
org=neovim
repo=$org
cmd=nvim



# ----------------------------------------------------------------------- Install Neovim {{{1


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



# --------------------------------------------------------- Install vim-plug and plugins {{{1


plug_vim="$HOME/.config/nvim/autoload/plug.vim"
if [ ! -f "$plug_vim" ]; then
  mkdir -p "$(dirname "$plug_vim")"
  curl -fLo "$plug_vim" \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

# Make sure Tree-sitter installs from scratch, existing files/dirs may cause errors.
share="$prefix"/share/nvim
find "$share" -maxdepth 1 -name tree-sitter\* -exec rm -rf {} \;
# Use only head of init.vim file containing plugin declarations relevant for PlugInstall
# -- until the plugins are available, the rest might cause errors which will abort the
# installation process.
sed '/call plug#end()/q' "$script_dir/init.vim" |
  nvim -u /dev/stdin +'PlugInstall --sync' +qall
nvim +UpdateEverything
