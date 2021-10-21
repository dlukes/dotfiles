solus_profile=/usr/share/defaults/etc/profile
if [ -f "$solus_profile" ]; then
  source "$solus_profile"
else
  PS1='\[\033[38;5;081m\]\u\[\033[38;5;245m\]@\[\033[38;5;206m\]\H \[\033[38;5;245m\]\w \[\033[38;5;081m\]$ \[\e[0m\]'
fi

export PATH="$HOME/bin:$HOME/.local/bin:$HOME/.linuxbrew/bin:/usr/local/bin:$PATH"
command -v rustc &>/dev/null && export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
export GOPATH="$HOME/src/go"
export HISTCONTROL=ignoreboth:erasedups
export TERMINFO_DIRS=/etc/terminfo:/lib/terminfo:/usr/share/terminfo

alias cp='cp -i'
alias crontab='EDITOR=vi crontab'
alias df='df -kh'
alias du='du -kh'
alias g=git
alias gc='git commit --verbose'
alias gco='git checkout'
alias gl='git log --topo-order --pretty=format:"${_git_log_medium_format}"'
alias glb='git log --topo-order --pretty=format:"${_git_log_brief_format}"'
alias glc='git shortlog --summary --numbered'
alias gld='git log --topo-order --stat --patch --full-diff --pretty=format:"${_git_log_medium_format}"'
alias glg='git log --topo-order --all --graph --pretty=format:"${_git_log_oneline_format}"'
alias glo='git log --topo-order --pretty=format:"${_git_log_oneline_format}"'
alias gls='git log --topo-order --stat --pretty=format:"${_git_log_medium_format}"'
alias gp='git push'
alias gs='git stash'
alias grep='grep --color=auto'
alias l='ls -1A'
alias la='ll -A'
alias ll='ls -GlrthF'
alias ls='ls --group-directories-first --color=auto'
alias mkdir='mkdir -p'
alias mv='mv -i'
alias nice='nice -n19 ionice -c3'
alias o=xdg-open
alias p="$PAGER"
alias py=python
alias rm='rm -i'
alias rsync-copy='rsync --verbose --progress --human-readable --compress --archive --hard-links --one-file-system --acls --xattrs'
alias rsync-move='rsync --verbose --progress --human-readable --compress --archive --hard-links --one-file-system --acls --xattrs --remove-source-files'
alias rsync-synchronize='rsync --verbose --progress --human-readable --compress --archive --hard-links --one-file-system --acls --xattrs --update --delete'
alias rsync-update='rsync --verbose --progress --human-readable --compress --archive --hard-links --one-file-system --acls --xattrs --update'
alias type='type -a'
if command -v nvim &>/dev/null; then
  alias vi=nvim
else
  alias vi=vim
fi
