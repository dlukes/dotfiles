function fish_prompt --description 'Write out the prompt'
  # if using WSL, inform Windows Terminal about PWD
  if type -q wslpath
    printf '\e]9;9;%s\e\\' (wslpath -m $PWD)
  end

  # Inform tmux about PWD via OSC 7. NOTE: Fish already sets OSC 7 for the benefit of
  # VTE-based and other terminal emulators in __fish_config_interactive.fish, but that's
  # conditioned on some env vars that might not be set in SSH sessions, plus it uses
  # a file:// URL which tmux currently doesn't seem to understand. On the other hand,
  # the non-URL variant doesn't seem to do it for VTE-based terminals like Tilix. So
  # make sure to override it in tmux, and only there.
  if string match -qr tmux $TERM
    printf "\e]7;$PWD\e\\"
  end

  # make sure cursor is bar
  printf '\e[5 q'

  test $SSH_TTY
    and printf (set_color red)$USER(set_color brwhite)'@'(set_color yellow)(prompt_hostname)' '
  test $USER = 'root'
    and echo -n (set_color red)'# '

  if set -q CONDA_DEFAULT_ENV; and test $CONDA_DEFAULT_ENV != __base__
    set cenv (set_color green)':'$CONDA_DEFAULT_ENV
  else
    set cenv ''
  end

  if set -q VIRTUAL_ENV
    set venv (basename $VIRTUAL_ENV)
    set venv (set_color yellow)':'(string replace -r -- '-.*?-py' '…' $venv)
  else
    set venv ''
  end

  echo -n (set_color cyan)(prompt_pwd)$cenv$venv \
    (set_color red)'❯'(set_color yellow)'❯'(set_color green)'❯ '(set_color normal)
end

# Reset cursor to block before entering Vim, which is hard to use with a bar cursor.
function set_cursor_to_block --on-event fish_preexec
  set -l prg (string split -r -m1 / (string split ' ' $argv)[1])[-1]
  if string match -qr '^vim?$' $prg &>/dev/null
    printf '\e[2 q'
  end
end
