#!/usr/bin/env zsh

set -e
source ${0:a:h}/util.sh

if is_macos; then
  brew_install_or_upgrade neovim
else # ------------------------------------------------------ START LINUX BRANCH

# Warn if competing install of nvim found.

on_path=$( command -v nvim || echo NVIM_NOT_FOUND )
in_local=$HOME/.local/bin/nvim
if [[ $on_path == NVIM_NOT_FOUND ]]; then
  echo "No previous nvim version found."
elif [[ $on_path != $in_local ]]; then
  read -q "YN?You're currently using $on_path but installing $in_local, are you sure?"
fi

# Compare versions and install / update / keep as is.

if [[ $on_path == NVIM_NOT_FOUND ]]; then
  old_version=NVIM_NOT_FOUND
else
  old_version=$( $on_path --version | head -n 1 | grep -oP '[\d\.]+' )
fi
new_version=$(
  # The latest release should be a stable version, not a pre-release.
  curl --silent https://api.github.com/repos/neovim/neovim/releases/latest |
    grep -oPm1 'NVIM v?[\d\.]+' |
    cut -d: -f2 |
    grep -oP '[\d\.]+'
)

install() {
  cd $( mktemp -d )
  wget https://github.com/neovim/neovim/releases/download/stable/nvim.appimage
  chmod +x nvim.appimage
  mv -f nvim.appimage $in_local.appimage
  ln -sf nvim.appimage $in_local
}

if [[ $old_version == NVIM_NOT_FOUND ]]; then
  install
  echo "Installed nvim $new_version."
elif [[ $old_version != $new_version ]]; then
  install
  echo "Updated nvim $old_version to $new_version."
else
  echo "Keeping up-to-date nvim $old_version."
fi

fi # ---------------------------------------------------------- END LINUX BRANCH

# Install/upgrade pynvim.

pip3 install --user --upgrade --upgrade-strategy eager pynvim

# Install vim-plug and plugins.

plug_vim=$HOME/.config/nvim/autoload/plug.vim
if [[ ! -f $plug_vim ]]; then
  curl -fLo $plug_vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

# use only head of init.vim file containing plugin declarations relevant
# for PlugInstall -- until the plugins are available, the rest might
# cause errors which will abort the installation process
nvim -u <( sed '/call plug#end()/q' ${0:a:h}/config/nvim/init.vim ) \
  -c 'PlugInstall --sync | qall'
