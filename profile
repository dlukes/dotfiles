# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login exists.

export main=$(locale -a | grep -iE 'en_us.utf-?8')
export alt=$(locale -a | grep -iE 'en_gb.utf-?8')
if [ -z "$alt" ]; then
  alt=$main
fi
. "$HOME/.files/locale"
unset main alt

# For Emacs TRAMP, just use the bare minimum necessary.
if [ "$TERM" = dumb ]; then
  # PATH needs to be set and exported separately for Bourne/POSIX compatibility, see
  # https://stackoverflow.com/a/10464554.
  PATH="$HOME/.local/mambaforge/envs/umrk/bin:$HOME/.local/bin:$HOME/.cargo/bin:$PATH"
  export PATH
# Otherwise, load interactive Bash config.
elif [ -n "$BASH_VERSION" ] && [ -f "$HOME/.bashrc" ]; then
  . "$HOME/.bashrc"
fi

# vi: set ft=sh:
