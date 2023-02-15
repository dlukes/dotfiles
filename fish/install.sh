#!/bin/bash

set -eufo pipefail
script_dir=$(dirname "$(realpath "$0")")
. "$script_dir"/../misc/util.sh

# TODO: AppImage install once https://github.com/fish-shell/fish-shell/issues/6475 is
# resolved. Some promising advances have been made here:
# https://github.com/fish-shell/fish-shell/issues/6475#issuecomment-1165396972

info 'Generating completions.'

if command -v podman >/dev/null; then
  podman completion --file "$HOME"/.config/fish/completions/podman.fish fish
fi
