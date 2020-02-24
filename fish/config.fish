set -gx LC_ALL en_US.utf-8
set -gx XDG_CONFIG_HOME ~/.config

set -l path \
  ~/.poetry/bin \
  ~/.local/pyenv/bin \
  ~/.cargo/bin \
  ~/.local/bin \
  ~/.files/bin \
  /usr/local/Cellar/{coreutils,gnu-tar,grep,gawk,gnu-sed,findutils}/**/gnubin
for p in $path[-1..1]
  if not contains $p $PATH
    set -gxp PATH $p
  end
end

# update database of frecently visited directories/files
if type -q fasd
  function update_fasd_db --on-event fish_preexec
    fasd --proc (fasd --sanitize $argv) &>/dev/null
  end
end

# python
set -gx VIRTUAL_ENV_DISABLE_PROMPT 1
if not set -q PYENV_ROOT
  set -gx PYENV_ROOT ~/.local/pyenv
  pyenv init - | source
end

# fzf
source ~/.config/nvim/plugged/fzf/shell/key-bindings.fish
fzf_key_bindings
if type -q fd
  set -gx FZF_CTRL_T_COMMAND 'fd --type f --hidden --follow --exclude .git'
end
if type -q bat
  set -gx FZF_CTRL_T_OPTS '--multi --preview "bat --style numbers,changes --color=always --decorations=always {} | head -500"'
end

# git
set -g __fish_git_prompt_showcolorhints
set -g __fish_git_prompt_use_informative_chars
# indicate we're in sync with upstream by just being silent
set -g __fish_git_prompt_char_upstream_equal ''

# may be slow in large repos, consider disabling it in them with
# git config --local bash.showInformativeStatus false
# set -g __fish_git_prompt_show_informative_status
# this is a subset which is faster and roughly equivalent to what I had
# in zsh
set -g __fish_git_prompt_showdirtystate
set -g __fish_git_prompt_showuntrackedfiles
set -g __fish_git_prompt_showupstream
set -g __fish_git_prompt_showstashstate

# bat
set -gx BAT_CONFIG_PATH ~/.files/bat.conf

# pre-load ssh keys
if type -q ssh-agent
  set -l ssh_agent_env /tmp/ssh-agent.fishenv.(id -u)

  if not set -q SSH_AUTH_SOCK
    test -r $ssh_agent_env && source $ssh_agent_env

    if not ps -U $LOGNAME -o pid,ucomm | grep -q -- "$SSH_AGENT_PID ssh-agent"
      # use the -t switch (e.g. -t 10m) to add a timeout on the auth
      eval (ssh-agent -c | sed '/^echo /d' | tee $ssh_agent_env)
    end
  end

  if ssh-add -l 2>&1 | grep -q 'The agent has no identities'
    ssh-add ~/.ssh/id_rsa 2>/dev/null
  end
end

# custom key bindings
bind \cx expand_glob
