#!/bin/sh

set -e

if command -v doom 2>&2; then
  doom upgrade
  doom build
  exit
fi

# install in XDG_CONFIG_HOME, but link to HOME nonetheless, as
# XDG_CONFIG_HOME might not be set when launching Emacs from the GUI
if [ -z "$XDG_CONFIG_HOME" ]; then
  export XDG_CONFIG_HOME="$HOME/.config"
fi
export DOOMDIR="$XDG_CONFIG_HOME/doom"
emacs_config="$XDG_CONFIG_HOME/emacs"
emacs_d="$HOME/.emacs.d"
doom_d="$HOME/.doom.d"

rm -rf "$emacs_config"
rm -rf "$emacs_d"
rm -rf "$DOOMDIR"
rm -rf "$doom_d"
mkdir -p "$XDG_CONFIG_HOME"

git clone https://github.com/hlissner/doom-emacs "$emacs_config"
ln -s "$emacs_config" "$emacs_d"
"$emacs_config"/bin/doom install
ln -s "$DOOMDIR" "$doom_d"
ln -sft ~/.local/bin "$emacs_config"/bin/doom
