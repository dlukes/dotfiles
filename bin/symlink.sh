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
  if [ "$basename" == "snippets" ]; then
    basename=UltiSnips
  fi

  if [ "$directory" = "$HOME" ] && [ "$basename" != "texmf" ]; then
    printf "$directory/.$basename"
  elif [ "$directory" = / ]; then
    printf "/.$basename"
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

#-----------------------------------------------------------------------
# Stuff belonging under $XDG_CONFIG_HOME
#-----------------------------------------------------------------------

if [ -z "$XDG_CONFIG_HOME" ]; then
  XDG_CONFIG_HOME="$HOME/.config"
fi

$action "$XDG_CONFIG_HOME" fish git python/flake8 python/pylintrc
# for Neovim, don't symlink the whole directory, most of it will be
# plugins and other auto-generated files, and it makes sense for those
# to be on the local filesystem on CNC servers (for faster access)
$action "$XDG_CONFIG_HOME/nvim" nvim/init.vim
$action "$XDG_CONFIG_HOME/nvim" snippets
$action "$XDG_CONFIG_HOME/nvim/lua" nvim/init.lua

#-----------------------------------------------------------------------
# Stuff belonging under $HOME
#-----------------------------------------------------------------------

$action "$HOME" bashrc editorconfig sqliterc texmf tmux.conf

#-----------------------------------------------------------------------
# Editorconfig under / if I'm an admin
#-----------------------------------------------------------------------

if groups | grep -wq sudo; then
  orig_owner=$( stat -c %U / )
  sudo chown "$user" /
  $action / editorconfig
  sudo chown "$orig_owner" /
fi
