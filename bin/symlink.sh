#!/bin/sh

set -euf
dirname=$(dirname "$0")
root=$(realpath $(dirname "$dirname"))
user=$(whoami)
. "$root"/misc/util.sh
cd "$root"

if [ -n "${DOTFILES_UNLINK:-}" ]; then
  action=delete_links
else
  action=create_links
fi

get_link_name() {
  local directory="$1"
  local target="$2"
  local basename=$(basename "$target")
  if [ "$basename" = "snippets" ]; then
    basename=UltiSnips
  fi

  if [ "$directory" = "$HOME" ]; then
    printf "$directory/.$basename"
  elif [ "$basename" = editorconfig ]; then
    printf "$directory/.$basename"
  else
    printf "$directory/$basename"
  fi
}

create_links() {
  local directory="$1"
  shift
  # $@ now contains the targets
  mkdir -p "$directory"
  local target
  for target in "$@"; do
    target="$root/$target"
    local link_name=$(get_link_name "$directory" "$target")
    if [ ! -e "$target" ]; then
      >&2 echo "WARNING: Not linking non-existent target: $target"
    elif [ ! -e "$link_name" ] || [ -L "$link_name" ]; then
      >&2 echo "Symlinking $target to $link_name"
      ln -sfn "$target" "$link_name"
    else
      >&2 echo "WARNING: Not overwriting non-symlink path: $link_name"
    fi
  done
}

delete_links() {
  local directory="$1"
  shift
  # $@ now contains the targets
  local target
  for target in "$@"; do
    target="$root/$target"
    local link_name=$(get_link_name "$directory" "$target")
    if [ -L "$link_name" ]; then
      >&2 echo "Removing symlink: $link_name"
      rm "$link_name"
    elif [ ! -e "$link_name" ]; then
      break
    else
      >&2 echo "WARNING: Not removing non-symlink path: $link_name"
    fi
    # TODO: if parent directory empty, remove it? but it will probably
    # never be the case...
  done
}

with_sudo() {
  local target="$2"
  local orig_owner=$(stat -c %U "$target")
  local orig_perms=$(stat -c %a "$target")
  sudo chown "$user" "$target"
  sudo chmod +w "$target"
  "$@"
  sudo chown "$orig_owner" "$target"
  sudo chmod $orig_perms "$target"
}

>&2 echo 'Original permissions on /:'
ls -ld /

#-----------------------------------------------------------------------
# Stuff belonging under $XDG_CONFIG_HOME
#-----------------------------------------------------------------------

true "${XDG_CONFIG_HOME:=$HOME/.config}"

$action "$XDG_CONFIG_HOME" emacs/doom fish git python/pylintrc python/matplotlib latexmk stylua.toml starship.toml direnv python/ruff
# for Neovim, don't symlink the whole directory, most of it will be
# plugins and other auto-generated files, and it makes sense for those
# to be on the local filesystem on CNC servers (for faster access)
$action "$XDG_CONFIG_HOME/nvim" nvim/init.lua snippets
# Same for containers.
$action "$XDG_CONFIG_HOME/containers" containers/containers.conf
# uv also puts some other files in its config dir, so don't symlink the
# full dir.
$action "$XDG_CONFIG_HOME/uv" python/uv/uv.toml

#-----------------------------------------------------------------------
# Stuff belonging under $XDG_CONFIG_HOME or macOS equivalent
#-----------------------------------------------------------------------

# if is_macos; then
#   config_dir="$HOME/Library/Application Support"
# else
#   config_dir="$XDG_CONFIG_HOME"
# fi
# Ruff should now be under ~/.config too, this former location is deprecated.
# $action "$config_dir" python/ruff

#-----------------------------------------------------------------------
# Stuff belonging under $XDG_DATA_HOME
#-----------------------------------------------------------------------

true ${XDG_DATA_HOME:=$HOME/.local/share}

$action "$XDG_DATA_HOME" pandoc

#-----------------------------------------------------------------------
# Stuff belonging under $HOME
#-----------------------------------------------------------------------

$action "$HOME" profile bashrc editorconfig sqliterc tmux.conf mamba/condarc R/lintr

#-----------------------------------------------------------------------
# IPython/Jupyter
#-----------------------------------------------------------------------

$action "$HOME/.ipython/profile_default" python/ipython_config.py

#-----------------------------------------------------------------------
# Editorconfig under / if I'm an admin
#-----------------------------------------------------------------------

if am_admin; then
  if [ "$(uname)" = Darwin ]; then
    # / is read-only on macOS, Homebrew already has an editorconfig and overwriting it
    # would complicate pulling, but it can still be useful to have your editorconfig
    # applied to files in the Cellar. E.g. Elisp files shipped with Emacs will format
    # nicer thanks to this.
    target=/opt/homebrew/Cellar
  else
    target=/
  fi
  with_sudo $action $target editorconfig
fi

#-----------------------------------------------------------------------
# Fontconfig
#-----------------------------------------------------------------------

# Ideally, I'd just symlink the entire directory to ~/.config/fontconfig, but
# unfortunately, it seems like the settings I need to tweak need to be loaded really
# early on, earlier than the user config kicks in (triggered by
# /etc/fonts/conf.d/50-user.conf), so let's symlink directly under /etc/fonts/conf.d
# instead.

if am_admin && [ "$(uname)" = Linux ]; then
  target=/etc/fonts/conf.d
  with_sudo $action $target fontconfig/00-overrides.conf
fi

>&2 echo 'All done. Double-check that permissions on / are the same as before:'
ls -ld /
