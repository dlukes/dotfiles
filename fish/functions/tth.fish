function tth --wraps ssh --description 'ssh + tmux'
  ssh -t $argv 'SHELL=~/.local/bin/fish tmux new -AD -s default'
end
