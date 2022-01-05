#!/bin/sh

set -eu
script_dir=$(dirname "$(realpath "$0")")
. "$script_dir"/util.sh
repo=Kitware/CMake

>&2 echo ">>> Installing $repo..."
cmake_installer=$(maybe_fetch_archive cmake $repo 'cmake-[\d.]+-linux-x86_64\.sh')
if [ -n "$cmake_installer" ]; then
  prefix="$HOME"/.local
  subdir="$prefix"/"${cmake_installer%.sh}"
  chmod +x "$cmake_installer"
  ./"$cmake_installer" --skip-license --prefix="$prefix" --include-subdir
  for bin in "$subdir"/bin/*; do
    ln -sft "$prefix"/bin "$bin"
  done
  rm "$cmake_installer"
  >&2 echo ">>> Installed $repo."
else
  >&2 echo ">>> $repo is already at newest version."
fi
