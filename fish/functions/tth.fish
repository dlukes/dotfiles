function tth --wraps ssh --description 'ssh + tmux'
  set -l ssh_argv
  set -l tmux_session_name default
  for arg in $argv
    if string match ':*' -- $arg >/dev/null
      set tmux_session_name (string sub -s 2 $arg)
    else
      set -a ssh_argv $arg
    end
  end
  ssh -t $ssh_argv "SHELL=~/.local/bin/fish tmux -u new -AD -s $tmux_session_name"
end
