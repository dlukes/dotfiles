#!/bin/sh

dirname=$( dirname "$0" )
root=$( realpath $( dirname "$dirname" ) )
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
  if [ "$directory" = "$HOME" ] && [ "$basename" != "texmf" ]; then
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

#-----------------------------------------------------------------------
# Stuff belonging under $XDG_CONFIG_HOME
#-----------------------------------------------------------------------

if [ -z "$XDG_CONFIG_HOME" ]; then
  XDG_CONFIG_HOME="$HOME/.config"
fi

$action "$XDG_CONFIG_HOME" fish git python/flake8
# for Neovim, only init.vim is kept under version control, so it's
# better to symlink just this one file, to make sure the rest (plugins
# etc.) are on the local filesystem on CNC servers (for faster access)
$action "$XDG_CONFIG_HOME/nvim" nvim/init.vim

#-----------------------------------------------------------------------
# Stuff belonging under $HOME
#-----------------------------------------------------------------------

$action "$HOME" bashrc editorconfig sqliterc texmf tmux.conf

#-----------------------------------------------------------------------
# Emacs/Spacemacs
#-----------------------------------------------------------------------

$action "$HOME" emacs/spacemacs emacs/emacs.d
$action "$root/emacs/emacs.d/private/snippets" emacs/snippets/*
