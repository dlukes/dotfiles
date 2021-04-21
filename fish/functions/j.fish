function j --description 'Jump to directory with fasd'
  # NOTE: interactive selection is implemented in ../completions/j.fish
  set -l dir (fasd -dlR $argv[1] | head -1)
  if test -d "$dir"
    cd $dir
  else
    cd $argv[1]
  end
end
