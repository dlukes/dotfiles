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

  if set -q CONDA_DEFAULT_ENV; and test $CONDA_DEFAULT_ENV != base
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
