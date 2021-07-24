set -gx LANG en_US.utf-8
set -gx LC_ALL $LANG
set -gx XDG_CONFIG_HOME ~/.config

# ------------------------------------------------------------- Homebrew {{{1

if not set -q HOMEBREW_PREFIX
  set -l brew_prefix /opt/homebrew
  set -l brew $brew_prefix/bin/brew
  set -l cellar $brew_prefix/Cellar
  if type -q $brew
    $brew shellenv | source
    if test -d $cellar
      set -gxp PATH $cellar/{coreutils,gnu-tar,grep,gawk,gnu-sed,findutils}/**/gnubin
    end
  else
    set -gx HOMEBREW_PREFIX
  end
end

# --------------------------------------------------------- Custom paths {{{1

if not set -q CUSTOM_PATHS
  set -gx CUSTOM_PATHS
  set -gxp PATH ~/.local/bin ~/.files/bin ~/.cargo/bin
end

# MANPATH is either correctly initiliazed above by Homebrew, or below
if not set -q MANPATH
  # empty element is needed so that joined MANPATH ends with :, which
  # means system-wide locations for man pages will still be searched
  # even though MANPATH is set (cf. manpath command)
  set -gx MANPATH ''
end

# --------------------------------------------------------------- Python {{{1

set -gx PYTHONFAULTHANDLER 1
set -gx PYTHONBREAKPOINT ipdb.set_trace
set -gx PYTHONSTARTUP ~/.files/python/startup.py
set -gx VIRTUAL_ENV_DISABLE_PROMPT 1
set -gx POETRY_VIRTUALENVS_IN_PROJECT 1
if type -q pyenv
  if not set -q PYENV_ROOT
    set -gx PYENV_ROOT ~/.local/pyenv
    pyenv init --path | source
    set -gxp MANPATH (printf "%s\n" $PYENV_ROOT/versions/*/share/man | sort -Vr)
  end
  pyenv init - | source
end

# ----------------------------------------------------------------- Rust {{{1

if not set -q RUSTUP_HOME
  # this is the default value, so setting it is technically redundant,
  # but I'm using it as a sentinel, so I set it anyway
  set -gx RUSTUP_HOME ~/.rustup
  set -gxp MANPATH (printf "%s\n" $RUSTUP_HOME/toolchains/*/share/man | sort -r)
end

# ----------------------------------------------------------------- fasd {{{1

# update database of frecently visited directories/files
if type -q fasd
  function update_fasd_db --on-event fish_preexec
    fasd --proc (fasd --sanitize $argv) &>/dev/null
  end
end

# ------------------------------------------------------------------ fzf {{{1

source ~/.local/share/fzf/key-bindings.fish
fzf_key_bindings
if type -q fd
  set -gx FZF_CTRL_T_COMMAND 'fd --type f --hidden --follow --exclude .git'
end
if type -q bat
  set -gx FZF_CTRL_T_OPTS '--multi --preview "bat --style numbers,changes --color=always --decorations=always {} | head -500"'
end

# ------------------------------------------------------------------ git {{{1

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

# ------------------------------------------------------------------ bat {{{1

set -gx BAT_CONFIG_PATH ~/.files/bat.conf

# ------------------------------------------------------------------ SSH {{{1

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

# -------------------------------------------------- Custom key bindings {{{1

bind \cx expand_glob

# vi: foldmethod=marker
