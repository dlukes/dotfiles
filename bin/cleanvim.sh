#!/bin/bash

set -eufo pipefail

rm -rf ~/.local/share/nvim
rm -rf ~/.local/lib64/nvim
rm -rf ~/.local/state/nvim/{kanagawa,neogit}
rm -rf ~/.cache/nvim
rm -rf ~/.config/nvim/plugin
