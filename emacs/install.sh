#!/bin/sh

set -euf
script_dir=$(dirname "$(realpath "$0")")
. "$script_dir"/../misc/util.sh

if [ -z "$XDG_CONFIG_HOME" ]; then
  export XDG_CONFIG_HOME="$HOME/.config"
fi

# Install / update Emacs.
if is_macos; then
  brew tap railwaycat/emacsmacport
  cask=emacs-mac-28-spacemacs-icon
  if brew ls --versions --cask $cask >/dev/null 2>&1; then
    brew upgrade --cask $cask
  else
    brew install --cask $cask
  fi

  # Make sure we can build ZeroMQ bindings for emacs-jupyter.
  brew install autoconf automake libtool pkg-config

  # This tap already provides Emacs compiled --with-native-comp, but as of mid-April
  # 2022, the builds feel a bit janky and actually slower than railwaycat's. Maybe just
  # wait for https://github.com/railwaycat/homebrew-emacsmacport/issues/274.
  # brew tap jimeh/emacs-builds
  # brew update --cask emacs-app || brew install --cask emacs-app
elif command -v dnf >/dev/null &2>&1; then
  export PYTHONWARNDEFAULTENCODING=
  # sudo dnf copr enable -y deathwish/emacs-pgtk-nativecomp
  sudo dnf in -by emacs
fi

# Make sure dead keys work: https://www.emacswiki.org/emacs/DeadKeys TODO: This doesn't
# seem to be necessary for the PGTK branch, as long as input methods are enabled. Once
# that branch stabilizes, you can remove it.
launcher=/usr/share/applications/emacs.desktop
if [ -f "$launcher" ]; then
  sudo sed -i 's/Exec=emacs/Exec=env XMODIFIERS= emacs/' "$launcher"
fi

# Remove unwanted emacsclient launcher.
# launcher=/usr/share/applications/emacsclient.desktop
# if [ -f "$launcher" ]; then
#   sudo rm "$launcher"
# fi

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
  git clone https://github.com/doomemacs/doomemacs "$emacs_d"
  doom="$emacs_d"/bin/doom
  "$doom" install
fi

>&2 echo "\
>>> All done. Remember you might need to run 'doom build' to recompile
installed packages in certain situations:

- If you change the major version of Emacs, as bytecode is generally not
  backwards compatible.
- If you modify the source code of installed packages manually (e.g.
  while debugging), because packages are loaded from .elc files, not
  .el. Use '-r' to rebuild only what you've changed."
