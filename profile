# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login exists.

export main=en_US.utf8
export alt=en_GB.utf8
if ! locale -a | grep $alt >/dev/null 2>&1; then
  alt=C.UTF-8
fi
. "$HOME/.files/locale"
unset main alt

if [ -n "$BASH_VERSION" ] && [ -f "$HOME/.bashrc" ]; then
  . "$HOME/.bashrc"
fi

# vi: set ft=sh:
