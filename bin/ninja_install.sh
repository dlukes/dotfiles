#!/bin/sh

set -euf
prefix="$HOME/.local"
script_dir=$(dirname "$0")
. "$script_dir/util.sh"

repo=ninja
if is_macos; then
  os=mac
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
