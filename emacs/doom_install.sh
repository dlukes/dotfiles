#!/bin/sh

set -e
script_dir=$(dirname "$(realpath "$0")")

if [ -z "$XDG_CONFIG_HOME" ]; then
  export XDG_CONFIG_HOME="$HOME/.config"
fi

# This is one of the *standard* DOOMDIR locations expected by Doom. If XDG_CONFIG_HOME
# is not set, Doom should default to ~/.config, so everything should work fine even
# without symlinking this to ~/.doom.d (the other standard location for DOOMDIR, which
# doesn't depend on XDG_CONFIG_HOME, but clutters the home directory).
export DOOMDIR="$XDG_CONFIG_HOME/doom"
if [ "$(realpath "$DOOMDIR")" != "$script_dir/doom" ]; then
  >&2 echo "Private Doom config not in place, run symlink.sh first."
  exit 1
fi

# Ditto for this location vs. ~/.emacs.d, except this depends on fairly recent support
# for XDG_CONFIG_HOME in Emacs itself. According to Emacs's etc/NEWS.27 file, it looks
# like XDG_CONFIG_HOME will also fall back to ~/.config if not set -- let's hope so.
#
# Ideally, if you want to test a vanilla Emacs config, just create an empty ~/.emacs.d
# and off you go.
emacs_d="$XDG_CONFIG_HOME/emacs"

if command -v doom >/dev/null 2>&1; then
  doom upgrade  # = cd ~/.emacs.d; git pull; doom clean, sync, update
else
  mkdir -p "$XDG_CONFIG_HOME"
  git clone https://github.com/hlissner/doom-emacs "$emacs_d"
  doom="$emacs_d"/bin/doom
  "$doom" install
  ln -sft ~/.local/bin "$doom"
fi

# Patch org-indent.el to behave correctly when org-indent-indentation-per-level is set
# to 0. TODO: Get rid of this when/if
# https://lists.gnu.org/archive/html/emacs-orgmode/2021-08/msg00166.html gets resolved.
org_indent=$(find "$emacs_d" -type f -name org-indent.el)
cd "$(dirname "$org_indent")"
git reset --hard
patch -up1 <"$script_dir"/org-indent.patch
doom build -r

# Make sure dead keys work: https://www.emacswiki.org/emacs/DeadKeys
launcher=/usr/share/applications/emacs.desktop
if [ -f "$launcher" ]; then
  sudo sed -i 's/Exec=/Exec=env XMODIFIERS= /' "$launcher"
fi

>&2 echo "\
All done. Remember you might need to run 'doom build' to recompile
installed packages in certain situations:

- If you change the major version of Emacs, as bytecode is generally not
  backwards compatible.
- If you modify the source code of installed packages manually (e.g.
  while debugging), because packages are loaded from .elc files, not
  .el. Use '-r' to rebuild only what you've changed."
