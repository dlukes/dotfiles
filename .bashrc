# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

SYSTEM=$( uname )

. "$HOME/bin/git-prompt.sh"
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_PS1_SHOWUPSTREAM="verbose"
export GIT_PS1_DESCRIBE_STYLE="branch"
export GIT_PS1_SHOWCOLORHINTS=1
export GIT_PS1_HIDE_IF_PWD_IGNORED=1

# If not running interactively, don't do anything
# [ -z "$PS1" ] && return

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

# ANSI color codes
RS="\[\033[0m\]"    # reset
HC="\[\033[1m\]"    # hicolor
UL="\[\033[4m\]"    # underline
INV="\[\033[7m\]"   # inverse background and foreground
FBLK="\[\033[30m\]" # foreground black
FRED="\[\033[31m\]" # foreground red
FGRN="\[\033[32m\]" # foreground green
FYEL="\[\033[33m\]" # foreground yellow
FBLE="\[\033[34m\]" # foreground blue
FMAG="\[\033[35m\]" # foreground magenta
FCYN="\[\033[36m\]" # foreground cyan
FWHT="\[\033[37m\]" # foreground white
BBLK="\[\033[40m\]" # background black
BRED="\[\033[41m\]" # background red
BGRN="\[\033[42m\]" # background green
BYEL="\[\033[43m\]" # background yellow
BBLE="\[\033[44m\]" # background blue
BMAG="\[\033[45m\]" # background magenta
BCYN="\[\033[46m\]" # background cyan
BWHT="\[\033[47m\]" # background white

if [ "$color_prompt" = yes ]; then
    # PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\] | \W$(__git_ps1 " (%s) ")\n\[\033[01;34m\]\@\[\033[00m\] λ '
    PROMPT_COMMAND='__git_ps1 "${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\] |" "\n\[\033[01;34m\]\@\[\033[00m\] λ "'
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# the same aliases as above, just without the test (OS X fails it for some
# reason and I'm too lazy to investigate...
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# some more ls aliases
alias ll='ls -GAlrthF'
alias la='ls -GA'
alias l='ls -GCF'
alias sudo='sudo env PATH="$PATH"'

# if vim is available, alias it to vi
[ -x $(which vim) ] && alias vi='vim'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# tab completion (only if shell is interactive, otherwise throws error)
if [[ $- == *i* ]]; then
    bind "set completion-ignore-case on"
    bind "set show-all-if-ambiguous on"
fi

# some kind of pager settings (?):
export LESS='-R'

# emacs settings
export EDITOR='emacsclient'
export ALTERNATE_EDITOR=''
alias et='TERM=xterm-256color emacsclient -ct -a ""'
# alias e='emacsclient -c -a ""'

# globstar
#         If set, the pattern ** used in a pathname expansion context will
#         match a files and zero or more directories and subdirectories.
#         If  the  pattern is followed by a /, only directories
#         and subdirectories match.
shopt -s globstar
# e.g. ls **/*.py
# shopt -s nullglob

# darwin-specific settings -- pandoc manpath, homebrew, MacPorts...
if [[ $SYSTEM == "darwin" ]]; then
    MANPATH="$HOME/Library/Haskell/ghc-7.6.3/lib/pandoc-1.12.4.2/share/man:$MANPATH"
    PATH="/usr/local/bin:/opt/local/bin:/opt/local/sbin:$PATH:/usr/local/cwb-3.4.8/bin"
fi

# add TeXLive to path and export it:
if [[ $SYSTEM == "Darwin" ]]; then
    PATH="/usr/texbin/:$PATH"
else
    PATH="/usr/local/texlive/2014/bin/x86_64-linux:$PATH"
    export MANPATH="/usr/local/texlive/2014/texmf-dist/doc/man:$MANPATH"
    export INFOPATH="/usr/local/texlive/2014/texmf-dist/doc/info:$INFOPATH"
fi

# coursera - Coding the Matrix
export COURSERA_EMAIL=dafydd.lukes@gmail.com
export COURSERA_PASS=v4Ar38WZjq

# git completion
source ~/bin/git-completion.bash

# Append additional directories to $PATH
# the custom install of texlive in ~ now hides anything installed by default by
# Ubuntu
PATH="$PATH:$HOME/local/bin:$HOME/htk/bin:$HOME/sbt/bin:$HOME/eclipse:$HOME/.cabal/bin:$HOME/Library/Haskell/bin"

# enable font anti-aliasing in JRE
export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=on -Xmx2048m"

export PYTHONPATH="$HOME/bin/python"
export PYTHONSTARTUP="$HOME/.pythonstartup"

# locate path for the database for my encrypted home folder
# export LOCATE_PATH="$HOME/var/mlocate.db"
# export DBPATH="$HOME/var/lib/mlocate/mlocate.db:$DBPATH"

# [ $SHLVL -eq 1 ] && eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)"

# path to OpenFST
PATH="$PATH:$HOME/local/openfst-1.4.1/src/bin"

# path to diplomka stuff:
PATH="$PATH:$HOME/Documents/dropbox/FFUK/DIP/experiment/bin"

# /sbin path (not added by default on openSUSE)
PATH="$PATH:/sbin"

# put home directory bin first on path
PATH="$HOME/bin:$PATH"

export PATH

export LC_ALL="en_US.utf-8"
export LANG="en_US.utf-8"

export MAILTO="dafydd.lukes@gmail.com"

function tth() {
    /usr/bin/ssh -t $@ "tmux attach || tmux new"
}

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
