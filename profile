# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login exists.

export main=$(locale -a | grep -iE 'en_us.utf-?8')
export alt=$(locale -a | grep -iE 'en_gb.utf-?8')
if [ -z "$alt" ]; then
  alt=$main
fi
. "$HOME/.files/locale"
unset main alt

if [ -n "$BASH_VERSION" ] && [ -f "$HOME/.bashrc" ]; then
  . "$HOME/.bashrc"
fi

# vi: set ft=sh:
