# ~/.bashrc: executed by bash(1) for non-login shells.

# Make sure PATH is correctly set when logging in via SSH. Important for picking the
# correct version of tmux when using tth, on remotes where I have a more recent version
# installed manually under ~/.local.
export PATH="$HOME/bin:$HOME/.local/bin:$HOME/.cargo/bin:$PATH"

# If not running interactively, don't do anything else.
case $- in
  *i*) ;;
    *) return;;
esac

# Fedora has this in /etc/skel/.bashrc, so better keep it around too.
if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

PS1='\[\033[38;5;081m\]\u\[\033[38;5;245m\]@\[\033[38;5;206m\]\H \[\033[38;5;245m\]\w \[\033[38;5;081m\]$ \[\e[0m\]'

HISTSIZE=1000
HISTFILESIZE=2000
HISTCONTROL=ignoreboth:erasedups
shopt -s histappend
shopt -s checkwinsize
shopt -s globstar

export TERMINFO_DIRS=/etc/terminfo:/lib/terminfo:/usr/share/terminfo

alias grep='grep --color=auto'
alias l='ls -1A'
alias ls='ls --group-directories-first --color=auto'
alias ll='ls -GlrthF'
alias la='ll -A'
if command -v nvim &>/dev/null; then
  alias vi=nvim
else
  alias vi=vim
fi
