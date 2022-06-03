#!/bin/sh

set -euf
script_dir=$(dirname "$(realpath "$0")")
. "$script_dir"/util.sh
prefix="$HOME"/.local

repo=ninja
if is_macos; then
  brew_install_or_upgrade ninja
  exit
else
  os=linux
fi
archive="$repo-$os.zip"
>&2 echo ">>> Installing $repo..."

download_url=$(
  curl -sSLf "https://api.github.com/repos/ninja-build/$repo/releases/latest" |
    grep -oPm1 "https://.*?/$archive"
)
cd "$prefix"/bin
curl -sSLfO "$download_url"
unzip -qqo "$archive"
rm "$archive"
>&2 echo ">>> Installed $repo."
