#  Customize BASH PS1 prompt to show current GIT repository and branch.
#  by Mike Stewart - http://MediaDoneRight.com

export PATH="$HOME/bin:$HOME/local/bin:$HOME/.linuxbrew/bin:$PATH"

#  SETUP CONSTANTS
#  Bunch-o-predefined colors.  Makes reading code easier than escape sequences.
#  I don't remember where I found this.  o_O


# Reset
Color_Off="\[\033[0m\]"       # Text Reset

# Regular Colors
Black="\[\033[0;30m\]"        # Black
Red="\[\033[0;31m\]"          # Red
Green="\[\033[0;32m\]"        # Green
Yellow="\[\033[0;33m\]"       # Yellow
Blue="\[\033[0;34m\]"         # Blue
Purple="\[\033[0;35m\]"       # Purple
Cyan="\[\033[0;36m\]"         # Cyan
White="\[\033[0;37m\]"        # White

# Bold
BBlack="\[\033[1;30m\]"       # Black
BRed="\[\033[1;31m\]"         # Red
BGreen="\[\033[1;32m\]"       # Green
BYellow="\[\033[1;33m\]"      # Yellow
BBlue="\[\033[1;34m\]"        # Blue
BPurple="\[\033[1;35m\]"      # Purple
BCyan="\[\033[1;36m\]"        # Cyan
BWhite="\[\033[1;37m\]"       # White

# Underline
UBlack="\[\033[4;30m\]"       # Black
URed="\[\033[4;31m\]"         # Red
UGreen="\[\033[4;32m\]"       # Green
UYellow="\[\033[4;33m\]"      # Yellow
UBlue="\[\033[4;34m\]"        # Blue
UPurple="\[\033[4;35m\]"      # Purple
UCyan="\[\033[4;36m\]"        # Cyan
UWhite="\[\033[4;37m\]"       # White

# Background
On_Black="\[\033[40m\]"       # Black
On_Red="\[\033[41m\]"         # Red
On_Green="\[\033[42m\]"       # Green
On_Yellow="\[\033[43m\]"      # Yellow
On_Blue="\[\033[44m\]"        # Blue
On_Purple="\[\033[45m\]"      # Purple
On_Cyan="\[\033[46m\]"        # Cyan
On_White="\[\033[47m\]"       # White

# High Intensty
IBlack="\[\033[0;90m\]"       # Black
IRed="\[\033[0;91m\]"         # Red
IGreen="\[\033[0;92m\]"       # Green
IYellow="\[\033[0;93m\]"      # Yellow
IBlue="\[\033[0;94m\]"        # Blue
IPurple="\[\033[0;95m\]"      # Purple
ICyan="\[\033[0;96m\]"        # Cyan
IWhite="\[\033[0;97m\]"       # White

# Bold High Intensty
BIBlack="\[\033[1;90m\]"      # Black
BIRed="\[\033[1;91m\]"        # Red
BIGreen="\[\033[1;92m\]"      # Green
BIYellow="\[\033[1;93m\]"     # Yellow
BIBlue="\[\033[1;94m\]"       # Blue
BIPurple="\[\033[1;95m\]"     # Purple
BICyan="\[\033[1;96m\]"       # Cyan
BIWhite="\[\033[1;97m\]"      # White

# High Intensty backgrounds
On_IBlack="\[\033[0;100m\]"   # Black
On_IRed="\[\033[0;101m\]"     # Red
On_IGreen="\[\033[0;102m\]"   # Green
On_IYellow="\[\033[0;103m\]"  # Yellow
On_IBlue="\[\033[0;104m\]"    # Blue
On_IPurple="\[\033[10;95m\]"  # Purple
On_ICyan="\[\033[0;106m\]"    # Cyan
On_IWhite="\[\033[0;107m\]"   # White

# Various variables you might want for your PS1 prompt instead
Time12h="\T"
Time12a="\@"
PathShort="\w"
PathFull="\W"
NewLine="\n"
Jobs="\j"


# This PS1 snippet was adopted from code for MAC/BSD I saw from: http://allancraig.net/index.php?option=com_content&view=article&id=108:ps1-export-command-for-git&catid=45:general&Itemid=96
# I tweaked it to work on UBUNTU 11.04 & 11.10 plus made it mo' better

export PS1=$IBlack$Time12h$Color_Off'$(git branch &>/dev/null;\
if [ $? -eq 0 ]; then \
  echo "$(echo `git status` | grep "nothing to commit" > /dev/null 2>&1; \
  if [ "$?" -eq "0" ]; then \
    # @4 - Clean repository - nothing to commit
    echo "'$Green'"$(__git_ps1 " (%s)"); \
  else \
    # @5 - Changes to working tree
    echo "'$IRed'"$(__git_ps1 " {%s}"); \
  fi) '$BYellow$PathShort$Color_Off' \$ "; \
else \
  # @2 - Prompt when not in GIT repo
  echo " '$Yellow$PathShort$Color_Off' \$ "; \
fi)'

# Use completion and prompt if available
# [[ -f /usr/share/bash-completion/bash_completion ]] && \
#   . /usr/share/bash-completion/bash_completion
# [[ -f /usr/share/git/completion/git-completion.bash ]] && \
#   . /usr/share/git/completion/git-completion.bash
[[ -f /usr/share/git/git-prompt.sh ]] && \
  . /usr/share/git/git-prompt.sh

alias 1='cd +1'
alias 2='cd +2'
alias 3='cd +3'
alias 4='cd +4'
alias 5='cd +5'
alias 6='cd +6'
alias 7='cd +7'
alias 8='cd +8'
alias 9='cd +9'
alias _=sudo
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e ''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'')"'
alias b='${(z)BROWSER}'
alias brewC='brew cleanup --force'
alias brewc='brew cleanup'
alias brewi='brew install'
alias brewl='brew list'
alias brews='brew search'
alias brewu='brew update && brew upgrade --all'
alias brewx='brew remove'
alias cask='brew cask'
alias caskC='brew cask cleanup'
alias caskc='brew cask cleanup --outdated'
alias caski='brew cask install'
alias caskl='brew cask list'
alias casks='brew cask search'
alias caskx='brew cask uninstall'
alias cp='cp -i'
alias crontab='EDITOR=vi crontab'
alias d='dirs -v'
alias df='df -kh'
alias du='du -kh'
alias e='${(z)VISUAL:-${(z)EDITOR}}'
alias g=git
alias gCO='gCo $(gCl)'
alias gCT='gCt $(gCl)'
alias gCa='git add $(gCl)'
alias gCe='git mergetool $(gCl)'
alias gCl='git status | sed -n "s/^.*both [a-z]*ed: *//p"'
alias gCo='git checkout --ours --'
alias gCt='git checkout --theirs --'
alias gR='git remote'
alias gRa='git remote add'
alias gRb=git-hub-browse
alias gRl='git remote --verbose'
alias gRm='git remote rename'
alias gRp='git remote prune'
alias gRs='git remote show'
alias gRu='git remote update'
alias gRx='git remote rm'
alias gS='git submodule'
alias gSI='git submodule update --init --recursive'
alias gSa='git submodule add'
alias gSf='git submodule foreach'
alias gSi='git submodule init'
alias gSl='git submodule status'
alias gSm=git-submodule-move
alias gSs='git submodule sync'
alias gSu='git submodule foreach git pull origin master'
alias gSx=git-submodule-remove
alias gb='git branch'
alias gbL='git branch -av'
alias gbM='git branch -M'
alias gbS='git show-branch -a'
alias gbX='git branch -D'
alias gbc='git checkout -b'
alias gbl='git branch -v'
alias gbm='git branch -m'
alias gbs='git show-branch'
alias gbx='git branch -d'
alias gc='git commit --verbose'
alias gcF='git commit --verbose --amend'
alias gcO='git checkout --patch'
alias gcP='git cherry-pick --no-commit'
alias gcR='git reset "HEAD^"'
alias gca='git commit --verbose --all'
alias gcf='git commit --amend --reuse-message HEAD'
alias gcl=git-commit-lost
alias gcm='git commit --message'
alias gco='git checkout'
alias gcp='git cherry-pick --ff'
alias gcr='git revert'
alias gcs='git show'
alias gd='git ls-files'
alias gdc='git ls-files --cached'
alias gdi='git status --porcelain --short --ignored | sed -n "s/^!! //p"'
alias gdk='git ls-files --killed'
alias gdm='git ls-files --modified'
alias gdu='git ls-files --other --exclude-standard'
alias gdx='git ls-files --deleted'
alias get='curl --continue-at - --location --progress-bar --remote-name --remote-time'
alias gf='git fetch'
alias gfc='git clone'
alias gfm='git pull'
alias gfr='git pull --rebase'
alias gg='git grep'
alias ggL='git grep --files-without-matches'
alias ggi='git grep --ignore-case'
alias ggl='git grep --files-with-matches'
alias ggv='git grep --invert-match'
alias ggw='git grep --word-regexp'
alias giA='git add --patch'
alias giD='git diff --no-ext-diff --cached --word-diff'
alias giR='git reset --patch'
alias giX='git rm -rf --cached'
alias gia='git add'
alias gid='git diff --no-ext-diff --cached'
alias gir='git reset'
alias giu='git add --update'
alias gix='git rm -r --cached'
alias gl='git log --topo-order --pretty=format:"${_git_log_medium_format}"'
alias glb='git log --topo-order --pretty=format:"${_git_log_brief_format}"'
alias glc='git shortlog --summary --numbered'
alias gld='git log --topo-order --stat --patch --full-diff --pretty=format:"${_git_log_medium_format}"'
alias glg='git log --topo-order --all --graph --pretty=format:"${_git_log_oneline_format}"'
alias glo='git log --topo-order --pretty=format:"${_git_log_oneline_format}"'
alias gls='git log --topo-order --stat --pretty=format:"${_git_log_medium_format}"'
alias gm='git merge'
alias gmC='git merge --no-commit'
alias gmF='git merge --no-ff'
alias gma='git merge --abort'
alias gmt='git mergetool'
alias gp='git push'
alias gpA='git push --all && git push --tags'
alias gpa='git push --all'
alias gpc='git push --set-upstream origin "$(git-branch-current 2> /dev/null)"'
alias gpf='git push --force'
alias gpp='git pull origin "$(git-branch-current 2> /dev/null)" && git push origin "$(git-branch-current 2> /dev/null)"'
alias gpt='git push --tags'
alias gr='git rebase'
alias gra='git rebase --abort'
alias grc='git rebase --continue'
alias grep='grep --color=auto'
alias gri='git rebase --interactive'
alias grs='git rebase --skip'
alias gs='git stash'
alias gsL=git-stash-dropped
alias gsS='git stash save --patch --no-keep-index'
alias gsX=git-stash-clear-interactive
alias gsa='git stash apply'
alias gsd='git stash show --patch --stat'
alias gsl='git stash list'
alias gsp='git stash pop'
alias gsr=git-stash-recover
alias gss='git stash save --include-untracked'
alias gsw='git stash save --include-untracked --keep-index'
alias gsx='git stash drop'
alias gwC='git clean -f'
alias gwD='git diff --no-ext-diff --word-diff'
alias gwR='git reset --hard'
alias gwS='git status --ignore-submodules=${_git_status_ignore_submodules}'
alias gwX='git rm -rf'
alias gwc='git clean -n'
alias gwd='git diff --no-ext-diff'
alias gwr='git reset --soft'
alias gws='git status --ignore-submodules=${_git_status_ignore_submodules} --short'
alias gwx='git rm -r'
alias history-stat='history 0 | awk ''{print $2}'' | sort | uniq -c | sort -n -r | head'
alias http-serve='python -m SimpleHTTPServer'
alias iperl='rlwrap -A -S "iperl> " perl -MData::Printer -wnE ''BEGIN { say "# Use `p @<arrayOrList>` or `p %<hashTable>` to print arrays/lists/hashtables; e.g.: `p %ENV`"; } say eval()//$@'''
alias j='fasd_cd -i'
alias l='ls -1A'
alias la='ll -A'
alias lc='lt -c'
alias lk='ll -Sr'
alias ll='ls -GAlrthF'
alias lm='la | "$PAGER"'
alias ln='ln -i'
alias lr='ll -R'
alias ls='ls --group-directories-first --color=auto'
alias lt='ll -tr'
alias lu='lt -u'
alias lx='ll -XB'
alias mkdir='mkdir -p'
alias mv='mv -i'
alias nice='nice -n19 ionice -c3'
alias o=xdg-open
alias p='${(z)PAGER}'
alias pbc=pbcopy
alias pbp=pbpaste
alias po=popd
alias pu=pushd
alias py=python
alias rb=ruby
alias rm='rm -i'
alias rsync-copy='rsync --verbose --progress --human-readable --compress --archive --hard-links --one-file-system --acls --xattrs'
alias rsync-move='rsync --verbose --progress --human-readable --compress --archive --hard-links --one-file-system --acls --xattrs --remove-source-files'
alias rsync-synchronize='rsync --verbose --progress --human-readable --compress --archive --hard-links --one-file-system --acls --xattrs --update --delete'
alias rsync-update='rsync --verbose --progress --human-readable --compress --archive --hard-links --one-file-system --acls --xattrs --update'
alias sl=ls
alias top=htop
alias type='type -a'
alias vi='vim -Nnu NONE'
alias which-command=whence
