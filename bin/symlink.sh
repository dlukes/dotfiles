#!/bin/sh

dirname=$( dirname "$0" )
root=$( realpath $( dirname "$dirname" ) )
user=$( whoami )
cd "$root"
if [ -n "$DOTFILES_UNLINK" ]; then
  action=delete_links
else
  action=create_links
fi

get_link_name() {
  directory="$1"
  target="$2"
  basename=$( basename "$target" )
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
  directory="$1"; shift
  # $@ now contains the targets
  mkdir -p "$directory"
  for target in "$@"; do
    target="$root/$target"
    link_name=$( get_link_name "$directory" "$target" )
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
  directory="$1"; shift
  # $@ now contains the targets
  for target in "$@"; do
    target="$root/$target"
    link_name=$( get_link_name "$directory" "$target" )
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
  target="$2"
  orig_owner=$( stat -c %U $target )
  sudo chown "$user" "$target"
  "$@"
  sudo chown "$orig_owner" "$target"
}

#-----------------------------------------------------------------------
# Stuff belonging under $XDG_CONFIG_HOME
#-----------------------------------------------------------------------

if [ -z "$XDG_CONFIG_HOME" ]; then
  XDG_CONFIG_HOME="$HOME/.config"
fi

$action "$XDG_CONFIG_HOME" containers emacs/doom fish git python/pylintrc
# for Neovim, don't symlink the whole directory, most of it will be
# plugins and other auto-generated files, and it makes sense for those
# to be on the local filesystem on CNC servers (for faster access)
$action "$XDG_CONFIG_HOME/nvim" nvim/init.vim
$action "$XDG_CONFIG_HOME/nvim" snippets
$action "$XDG_CONFIG_HOME/nvim/lua" nvim/init.lua

#-----------------------------------------------------------------------
# Stuff belonging under $HOME
#-----------------------------------------------------------------------

$action "$HOME" profile bashrc editorconfig sqliterc tmux.conf mamba/condarc

#-----------------------------------------------------------------------
# Editorconfig under / if I'm an admin
#-----------------------------------------------------------------------

if groups | grep -wqP 'sudo|admin'; then
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

if [ "$(uname)" = Linux ]; then
  target=/etc/fonts/conf.d
  with_sudo $action $target fontconfig/00-overrides.conf
fi
