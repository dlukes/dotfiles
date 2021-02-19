function fish_prompt --description 'Write out the prompt'
  # if using WSL, inform Windows Terminal about $PWD
  if type -q wslpath
    printf '\e]9;9;%s\e\\' (wslpath -m $PWD)
  end

  # make sure cursor is bar
  printf '\e[5 q'

  test $SSH_TTY
    and printf (set_color red)$USER(set_color brwhite)'@'(set_color yellow)(prompt_hostname)' '
  test $USER = 'root'
    and echo -n (set_color red)'# '

  if set -q VIRTUAL_ENV
    set venv (basename $VIRTUAL_ENV)
    set venv (set_color yellow)':'(string replace -r -- '-.*?-py' '…' $venv)
  else
    set venv ''
  end

  if type -q rustc
    # empty element is needed so that joined MANPATH ends with :, which
    # means system-wide locations for man pages will still be searched
    # even though MANPATH is set (cf. manpath command)
    set -gx MANPATH (rustc --print sysroot)/share/man ''
  end

  echo -n (set_color cyan)(prompt_pwd)$venv \
    (set_color red)'❯'(set_color yellow)'❯'(set_color green)'❯ '(set_color normal)
end
