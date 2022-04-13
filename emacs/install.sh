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
  brew install --cask emacs-mac-spacemacs-icon

  # This tap already provides Emacs compiled --with-native-comp, but as of mid-April
  # 2022, the builds feel a bit janky and actually slower than railwaycat's. Maybe just
  # wait for https://github.com/railwaycat/homebrew-emacsmacport/issues/274.
  # brew tap jimeh/emacs-builds
  # brew update --cask emacs-app || brew install --cask emacs-app
elif command -v dnf >/dev/null &2>&1; then
  export PYTHONWARNDEFAULTENCODING=
  sudo dnf copr enable deathwish/emacs-pgtk-nativecomp
  sudo dnf install emacs || dnf update emacs
fi

# Make sure dead keys work: https://www.emacswiki.org/emacs/DeadKeys
launcher=/usr/share/applications/emacs.desktop
if [ -f "$launcher" ]; then
  sudo sed -i 's/Exec=/Exec=env XMODIFIERS= /' "$launcher"
fi

# Remove unwanted emacsclient launcher.
launcher=/usr/share/applications/emacsclient.desktop
if [ -f "$launcher" ]; then
  sudo rm "$launcher"
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

# Until https://github.com/justinbarclay/parinfer-rust-mode/issues/43 is resolved,
# compile the parinfer-rust dylib manually on macOS because of lacking M1 support.
parinfer_so=$(find "$emacs_d" -name parinfer-rust-darwin.so)
if [ -f "$parinfer_so" ]; then
  >&2 echo '>>> Building parinfer-rust manually to make sure it has M1 support.'
  cd ~/.cache
  repo_name=parinfer-rust
  if ! [ -d "$repo_name" ]; then
    git clone --depth 1 https://github.com/eraserhd/"$repo_name".git
    cd "$repo_name"
  else
    cd "$repo_name"
    git fetch
  fi
  cargo build --release --features emacs
  cp target/release/libparinfer_rust.dylib "$parinfer_so"
fi

>&2 echo "\
>>> All done. Remember you might need to run 'doom build' to recompile
installed packages in certain situations:

- If you change the major version of Emacs, as bytecode is generally not
  backwards compatible.
- If you modify the source code of installed packages manually (e.g.
  while debugging), because packages are loaded from .elc files, not
  .el. Use '-r' to rebuild only what you've changed."
