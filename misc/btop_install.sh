#!/bin/sh

set -euf
script_dir=$(dirname "$(realpath "$0")")
. "$script_dir"/util.sh

if is_macos; then
  brew_install_or_upgrade btop
else
  user=aristocratos
  repo=btop
  ver=$(github_latest_release_tag_name $user $repo)
  archive=btop-x86_64-linux-musl.tbz
  prefix="$HOME"/.local
  cd "$(mktemp -d)"
  >&2 echo ">>> Installing btop. Temp dir is: $PWD"
  curl -sSLfOJ https://github.com/$user/$repo/releases/download/$ver/$archive
  tar xjf "$archive"
  rm "$archive"
  cd btop
  if am_admin; then
    # When updating, the existing executable will be owned by root due to 'make setuid',
    # so 'make install' needs root too in order to be able to replace it.
    sudo make install PREFIX="$prefix"
    sudo make setuid PREFIX="$prefix"
  else
    make install PREFIX="$prefix"
    >&2 echo ">>> No sudo privileges on this box, can't setuid on the binary."
  fi
fi
