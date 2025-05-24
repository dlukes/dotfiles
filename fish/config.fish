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
if type -q nvim
    set -gx EDITOR nvim
    set -gx VISUAL nvim
else
    set -gx EDITOR vim
    set -gx VISUAL vim
end
set -gx PAGER less
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

# --------------------------------------------------------------------------------- Fish {{{1

# Variables migrated over from the (universal) fish_variables file, which should not be
# in version control (it's optimal for machine-specific stuff).
set -g fish_color_autosuggestion 969896
set -g fish_color_cancel -r
set -g fish_color_command b294bb
set -g fish_color_comment f0c674
set -g fish_color_cwd green
set -g fish_color_cwd_root red
set -g fish_color_end b294bb
set -g fish_color_error cc6666
set -g fish_color_escape 00a6b2
set -g fish_color_history_current --bold
set -g fish_color_host normal
set -g fish_color_host_remote yellow
set -g fish_color_match --background=brblue
set -g fish_color_normal normal
set -g fish_color_operator 00a6b2
set -g fish_color_param 81a2be
set -g fish_color_quote b5bd68
set -g fish_color_redirection 8abeb7
set -g fish_color_search_match 'bryellow --background=brblack'
set -g fish_color_selection 'white --bold --background=brblack'
set -g fish_color_status red
set -g fish_color_user brgreen
set -g fish_color_valid_path --underline
set -g fish_greeting
set -g fish_key_bindings fish_default_key_bindings
set -g fish_pager_color_completion normal
set -g fish_pager_color_description 'B3A06D yellow'
set -g fish_pager_color_prefix 'white --bold --underline'
set -g fish_pager_color_progress 'brwhite --background=cyan'
set -g fish_pager_color_selected_background -r

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
set -q CONDA_EXE; or set -gx CONDA_EXE ~/.local/miniforge3/condabin/conda

if test -x $CONDA_EXE
    # By default, Conda adds prompt wrappers, which can then fail to preserve $status and
    # $pipestatus in your wrapped prompt functions (the left prompt wrapper tries to
    # preserve at least $status AFAICS, but the right prompt wrapper doesn't). You don't
    # need those wrappers, since you add Conda env info to your prompt manually anyway, so
    # just get rid of them.
    $CONDA_EXE shell.fish hook |
        awk '/^function __conda_add_prompt/{rm=1} /^function conda/{rm=0} !rm' |
        source
    set -l miniforge3_root (string replace -r '/bin/conda$' '' -- $CONDA_EXE)
    source $miniforge3_root/etc/fish/conf.d/mamba.fish
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
        ssh-add --apple-use-keychain ~/.ssh/id_rsa 2>/dev/null
    end
end

# ------------------------------------------------------------------------------- Aspell {{{1

# set -l aspell_dir ~/Desktop/data/aspell
# for dict in $aspell_dir/*.rws
#     set -gxa ASPELL_CONF "add-dict-alias $(string sub -e -4 -- $(basename $dict)) $dict;"
# end
# set -gxa ASPELL_CONF "personal $aspell_dir/personal;"

# ------------------------------------------------------------------ Custom key bindings {{{1

bind \cx expand_glob

# ---------------------------------------------------------------------- Starship prompt {{{1

starship init fish | source

# ------------------------------------------------------------------------------- Direnv {{{1

direnv hook fish | source

# --------------------------------------------------------------------------------- Java {{{1

# JAVA_HOME should be set to avoid confusing build failures where javac is found, but
# the correct JDK to use cannot be inferred. So set it to the latest available JDK.
set -l java_home /usr/libexec/java_home
if test -x $java_home
    set -gx JAVA_HOME ($java_home)
end

# -------------------------------------------------------------------------------- Abbrs {{{1

abbr -a -- plo 'podman logs'
abbr -a -- ffprobe 'ffprobe -hide_banner'
abbr -a -- sudof 'sudo -E fish -c'
abbr -a -- fda 'fd -HI'
abbr -a -- du 'du -kh'
abbr -a -- ssD 'sudo systemctl disable --now'
abbr -a -- gst 'git stash'
abbr -a -- giu 'git add --update'
abbr -a -- gc 'git commit --verbose'
abbr -a -- g git
abbr -a -- ppc 'podman container prune'
abbr -a -- sse 'sudo systemctl enable --now'
abbr -a -- ssr 'sudo systemctl restart'
abbr -a -- gia 'git add'
abbr -a -- gco 'git checkout'
abbr -a -- df 'df -kh'
abbr -a -- ffmpeg 'ffmpeg -hide_banner'
abbr -a -- sje 'sudo journalctl -exu'
abbr -a -- gpf 'git push --force-with-lease'
abbr -a -- findrm 'find -mindepth 1 -ignore_readdir_race -delete'
abbr -a -- glg git\ log\ --graph\ --pretty=format:\'\%Cred\%h\%Creset\ -\%C\(yellow\)\%d\%Creset\ \%s\ \%Cgreen\(\%cr\)\ \%Cblue\<\%an\>\%Creset\'\ --abbrev-commit\ --date=relative\ --all
abbr -a -- mma 'mamba activate'
abbr -a -- pcs 'podman start -lia'
abbr -a -- ibus-setup 'PYTHON=/usr/bin/python3 ibus-setup'
abbr -a -- giA 'git add --patch'
abbr -a -- gwD 'git diff --no-ext-diff --word-diff=color'
abbr -a -- ssf 'sudo systemctl reset-failed'
abbr -a -- gws 'git diff --no-ext-diff --no-textconv --stat'
abbr -a -- hm 'history merge'
abbr -a -- ip 'ip -c'
abbr -a -- psl 'ps --forest -ouser,pid,pgid,tpgid,sid,stat,tty,ignored,%cpu,%mem,wchan,cmd'
abbr -a -- mms 'mamba activate --stack'
abbr -a -- pcc 'podman create'
abbr -a -- gwd 'git diff --no-ext-diff'
abbr -a -- pdf 'podman system df'
abbr -a -- gis 'git diff --no-ext-diff --no-textconv --cached --stat'
abbr -a -- perl 'perl -CSAD -Mv5.14 -Mutf8 -Mwarnings -Mwarnings=FATAL,utf8'
abbr -a -- abrg 'abbr | grep'
abbr -a -- gfm 'git pull'
abbr -a -- gss 'git status'
abbr -a -- ppi 'podman image prune'
abbr -a -- ssg 'sudo systemd-cgtop'
abbr -a -- pps 'podman ps'
abbr -a -- gsb 'br --git-status'
abbr -a -- psr 'podman system reset'
abbr -a -- rga 'rg -uuu'
abbr -a -- sss 'sudo systemctl status'
abbr -a -- rgi 'rg -i'
abbr -a -- sjf 'sudo journalctl -fxu'
abbr -a -- gs 'git status -s'
abbr -a -- sj 'sudo journalctl'
abbr -a -- ss 'sudo systemctl'
abbr -a -- gid 'git diff --no-ext-diff --cached'
abbr -a -- ssR 'sudo systemctl reload'
abbr -a -- giD 'git diff --no-ext-diff --cached --word-diff=color'
abbr -a -- ssS 'sudo systemctl show'
abbr -a -- ssc 'sudo systemctl cat'
abbr -a -- ssd 'sudo systemctl daemon-reload'
abbr -a -- sst 'sudo systemctl stop'
abbr -a -- mmd 'mamba deactivate'
abbr -a -- ssv 'sudo systemd-analyze verify'
abbr -a -- sudoe 'sudo -E'
abbr -a -- gll 'git log --oneline --all --graph'
abbr -a -- vi nvim
abbr -a -- vimdiff nvim\ -du\ NONE\ +\'set\ mouse=a\ et\ rtp+=\~/.config/nvim/plugged/seoul256.vim\'\ +\'syntax\ on\'\ +\'colorscheme\ seoul256\'
abbr -a -- gp 'git push'
abbr -a -- mm mamba
abbr -a -- rgm rg -g "'**/master/**'"
abbr -a -- zed 'zed -n .'
abbr -a -- code 'code -n .'
set -l yt_dlp 'uvx --no-cache --from yt-dlp[default] yt-dlp'
abbr -a -- yt-dlp $yt_dlp
abbr -a -- download-mp3 "$yt_dlp --extract-audio --audio-format mp3"
abbr -a -- gcd 'git clone --depth 1'
abbr -a -- gcf 'git clone --filter=blob:none'

# vi: foldmethod=marker
