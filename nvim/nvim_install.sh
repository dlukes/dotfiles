#!/bin/sh

set -e
dirname=$( dirname "$0" )
. "$dirname/util.sh"

api=https://api.github.com/repos/neovim/neovim/releases
if [ "$1" = --nightly ]; then
  api=$api/tags/nightly
else
  # The latest release should be a stable version, not a pre-release.
  api=$api/latest
fi

# Warn if competing install of nvim found.

on_path=$( command -v nvim || echo NVIM_NOT_FOUND )
local_bin="$HOME/.local/bin"
in_local="$local_bin/nvim"
if [ "$on_path" = NVIM_NOT_FOUND ]; then
  echo 'No previous nvim version found.'
elif [ "$on_path" != "$in_local" ]; then
  read -p "You're currently using $on_path but installing $in_local, are you sure? (y/n) " yn
  [ "$yn" = 'y' ] || exit
fi

# Compare versions and install / update / keep as is.

mkdir -p "$local_bin"

if [ "$on_path" = NVIM_NOT_FOUND ]; then
  old_version=NVIM_NOT_FOUND
else
  old_version=$("$on_path" --version | grep -Pm1 '^NVIM v')
fi
new_version=$(curl -sf $api | grep -oPm1 'NVIM v[^"]+')

get_download_link() {
  curl -sf $api |
    grep -oPm1 '"browser_download_url": ".*'"$1" |
    cut -d' ' -f2 |
    tr -d \"
}

install() {
  if is_macos; then
    echo '

    Temporarily using Homebrew while waiting for Neovim to provide an Apple Silicon release asset (https://github.com/neovim/neovim/issues/13880).

    '
    brew upgrade --fetch-HEAD nvim
    # TODO: This will need updating along the lines of the else branch
    # when it gets uncommented.
    # archive_name=nvim-macos.tar.gz
    # nvim_dirname=nvim-osx64
    # cd "$HOME/.local"
    # rm -rf $nvim_dirname
    # curl -LO $github_release/$archive_name
    # tar xzf $archive_name
    # rm $archive_name
    # cd bin
    # ln -sf ../$nvim_dirname/bin/nvim
  else
    asset=nvim.appimage
    cd "$( mktemp -d )"
    curl -LO "$(get_download_link $asset)"
    chmod +x $asset
    mv -f $asset "$in_local"
  fi
}

if [ "$old_version" = NVIM_NOT_FOUND ]; then
  install
  echo "Installed $new_version."
elif [ "$old_version" != "$new_version" ]; then
  install
  echo "Updated $old_version to $new_version."
else
  echo "Keeping up-to-date $old_version."
fi

# Install/upgrade pynvim.

pip3 install --upgrade --upgrade-strategy eager pynvim

# Install vim-plug and plugins.

plug_vim="$HOME/.config/nvim/autoload/plug.vim"
if [ ! -f "$plug_vim" ]; then
  mkdir -p "$( dirname "$plug_vim" )"
  curl -fLo "$plug_vim" \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

# use only head of init.vim file containing plugin declarations relevant
# for PlugInstall -- until the plugins are available, the rest might
# cause errors which will abort the installation process
sed '/call plug#end()/q' "$dirname/../nvim/init.vim" |
  nvim -u /dev/stdin +'PlugInstall --sync' +qall
nvim +UpdateEverything
