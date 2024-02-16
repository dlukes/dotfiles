# Fish shell startup can be profiled using the following command:
#
#   fish --profile-startup /tmp/fish.profile -i -c exit
#
# After a cursory look, the slowest part seems to be Anaconda configuration, which is
# not really surprising.

set -l shell
if type -q getent
  set shell (getent passwd $USER)
else
  set shell (dscl . -read ~ UserShell)
end
if string match -qr '/fish$' $shell
  echo >&2 -n "\
WARNING: Don't use Fish as your login shell, you have tons of configuration, it might
lead to slowdowns or even hangs. For instance, on Wayland, login hangs because the
ssh-add command below waits forever for the SSH key passphrase. So instead, use a shell
which you have only minimal config for, e.g. Bash, and set a custom command in the
terminal emulator apps you use.

Exiting early without performing any additional configuration, to avoid any potential
issues. Run the command below and log out, then back in:

  chsh -s /bin/bash
"
  exit
end

set -gx XDG_CONFIG_HOME ~/.config
set -gxp TERMINFO_DIRS /etc/terminfo /lib/terminfo /usr/share/terminfo

set -l locales (locale -a)
set -x main (string match -ri 'en_us.utf-?8' $locales)
set -x alt (string match -ri 'en_gb.utf-?8' $locales)
if test -z $alt
  set -x alt $main
end
source ~/.files/locale
set -e main
set -e alt



# -------------------------------------------------------------------------------- macOS {{{1


if test (uname -s) = Darwin
  # Conda-forge's Clang can't find headers on macOS, at least not in Fish. Details here:
  # <https://github.com/conda-forge/compilers-feedstock/issues/6>, which seems to
  # indicate it's a matter of tweaking activation scripts. This has been done for Bash
  # and Zsh as of 2023-01. See https://developer.apple.com/forums/thread/122762 for
  # a workaround, which is implemented below and seems to work.
  if not set -q CFLAGS CXXFLAGS
    set -l macos_cflags -isysroot (xcrun --show-sdk-path) -I/usr/include -L/usr/lib
    set -gx CFLAGS $macos_cflags
    set -gx CXXFLAGS $macos_cflags
  end

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
end



# ------------------------------------------------------------------------- Custom paths {{{1


if not set -q CUSTOM_PATHS
  set -gx CUSTOM_PATHS
  set -gxp PATH ~/.local/bin ~/.files/bin ~/.cargo/bin ~/.config/emacs/bin
end

# MANPATH is either correctly initiliazed above by Homebrew, or below
if not set -q MANPATH
  # empty element is needed so that joined MANPATH ends with :, which
  # means system-wide locations for man pages will still be searched
  # even though MANPATH is set (cf. manpath command)
  set -gx MANPATH ''
end
# TODO: Maybe you don't have to muck around with MANPATH, or at least not as much, some
# of it should be inferred from PATH. Check if that's the case and trim down MANPATH
# edits. See: https://github.com/rust-lang/cargo/issues/2729#issuecomment-1017881732

# We don't want to use Fish as login shell, but we *do* want to let subprocesses know
# that they're running inside fish (e.g. Perl local::lib setup, Anaconda etc.).
set -gx SHELL (type -p fish)
if type -q nvim
  set -gx EDITOR nvim
else
  set -gx EDITOR vim
end



# ------------------------------------------------------------------------------- Python {{{1


set -gx PYTHONFAULTHANDLER 1
# See <https://www.python.org/dev/peps/pep-0597/>. If you explicitly want to use the
# current locale encoding, specify encoding="locale". Currently though, too many tools
# I often use scream at me, so let's leave this off for a while.
# set -gx PYTHONWARNDEFAULTENCODING 1
set -gx PYTHONBREAKPOINT ipdb.set_trace
set -gx PYTHONSTARTUP ~/.files/python/startup.py
set -gx PYTHONPYCACHEPREFIX ~/.cache/pycache
set -gx VIRTUAL_ENV_DISABLE_PROMPT 1
# This is the default location on Linux, use it on macOS too by setting the env var.
set -gx MATPLOTLIBRC $XDG_CONFIG_HOME/matplotlib
set -q NLTK_DATA; or set -gx NLTK_DATA ~/.local/share/nltk_data
set -q SEABORN_DATA; or set -gx SEABORN_DATA ~/.local/share/seaborn-data
set -q CONDA_EXE; or set -gx CONDA_EXE ~/.local/mambaforge/condabin/conda

if test -x $CONDA_EXE
  # By default, Conda adds prompt wrappers, which can then fail to preserve $status and
  # $pipestatus in your wrapped prompt functions (the left prompt wrapper tries to
  # preserve at least $status AFAICS, but the right prompt wrapper doesn't). You don't
  # need those wrappers, since you add Conda env info to your prompt manually anyway, so
  # just get rid of them.
  $CONDA_EXE shell.fish hook |
    awk '/^function __conda_add_prompt/{rm=1} /^function conda/{rm=0} !rm' |
    source
  set -l mambaforge_root (string replace -r '/bin/conda$' '' -- $CONDA_EXE)
  source $mambaforge_root/etc/fish/conf.d/mamba.fish
  mamba activate umrk
end

set -gx MODULAR_HOME ~/.modular
fish_add_path ~/.modular/pkg/packages.modular.com_mojo/bin
fish_add_path ~/.pixi/bin



# --------------------------------------------------------------------------------- Rust {{{1


# Available from 1.68 (see release notes), should become the default in Rust 1.70, but
# let's switch early for the welcome speedup.
set -gx CARGO_REGISTRIES_CRATES_IO_PROTOCOL sparse

if not set -q RUSTUP_HOME
  # this is the default value, so setting it is technically redundant,
  # but I'm using it as a sentinel, so I set it anyway
  set -gx RUSTUP_HOME ~/.rustup
  if test -d $RUSTUP_HOME
    set -gxp MANPATH (printf "%s\n" $RUSTUP_HOME/toolchains/*/share/man | sort -r)
  end
end



# --------------------------------------------------------------------------------- Perl {{{1


# These would be nice and strict, see, https://stackoverflow.com/a/6163129, but they
# break too much third-party code. And as I'm not planning to write any first-party Perl
# code, just grudgingly use good tools that happen to be written in Perl, they're
# a no-go.
# set -gx PERL_UNICODE SAD
# set -gx PERL5OPTS '-Mv5.14 -Mutf8 -Mwarnings -Mwarnings=FATAL,utf8'
# So let's set them as an abbrev instead?
set -l local_lib ~/.local/perl5
if test -d $local_lib
  perl -I"$local_lib"/lib/perl5 -Mlocal::lib="$local_lib" | source
end



# ---------------------------------------------------------------------------------- fzf {{{1


# fzf/key-bindings.fish exits early in non-interactive shells, before defining
# fzf_key_bindings, so let's skip FZF config completely in that case, to avoid a command
# not found error when calling fzf_key_bindings.
if status is-interactive
  source ~/.local/share/fzf/key-bindings.fish
  fzf_key_bindings
  if type -q fd
    set -gx FZF_CTRL_T_COMMAND 'fd --type f --hidden --follow --exclude .git'
  end
  if type -q bat
    set -gx FZF_CTRL_T_OPTS '--multi --preview "bat --style numbers,changes --color=always --decorations=always {} | head -500"'
  end
end



# ---------------------------------------------------------------------------------- git {{{1


set -g __fish_git_prompt_showcolorhints 1
set -g __fish_git_prompt_use_informative_chars 1
# indicate we're in sync with upstream by just being silent
set -g __fish_git_prompt_char_upstream_equal ''

# may be slow in large repos, consider disabling it in them with
# git config --local bash.showInformativeStatus false
# set -g __fish_git_prompt_show_informative_status
# this is a subset which is faster and roughly equivalent to what I had
# in zsh
set -g __fish_git_prompt_showdirtystate 1
set -g __fish_git_prompt_showuntrackedfiles 1
set -g __fish_git_prompt_showupstream 1
set -g __fish_git_prompt_showstashstate 1



# ---------------------------------------------------------------------------------- bat {{{1


set -gx BAT_CONFIG_PATH ~/.files/bat.conf



# ------------------------------------------------------------------------------- Zoxide {{{1


if type -q zoxide
  zoxide init fish --cmd j | source
end



# ---------------------------------------------------------------------------------- SSH {{{1


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



# ------------------------------------------------------------------------------- Aspell {{{1


set -l aspell_dir ~/Desktop/data/aspell
for dict in $aspell_dir/*.rws
  set -gxa ASPELL_CONF "add-dict-alias $(string sub -e -4 -- $(basename $dict)) $dict;"
end
set -gxa ASPELL_CONF "personal $aspell_dir/personal;"



# ------------------------------------------------------------------ Custom key bindings {{{1


bind \cx expand_glob

# vi: foldmethod=marker
