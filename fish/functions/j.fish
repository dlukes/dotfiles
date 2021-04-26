function j --description 'Jump to directory with fasd'
  # NOTE: interactive selection is implemented in ../completions/j.fish

  # This ensures, among other things, that once you've selected a valid
  # completion from fasd, it's not first re-used as an input to fasd
  # below, which might end up selecting a different (more frecent) entry
  # which also matches the chosen path (e.g. a subdir).
  if test -d "$argv[1]"
    cd $argv[1]
    return
  end

  set -l dir (fasd -dlR $argv[1] | head -1)
  if test -d "$dir"
    cd $dir
  else
    cd $argv[1]
  end
end
