function mv --wraps mv
  # if only one argument is provided, default to moving into $PWD
  if test (count $argv) -eq 1
    set -a argv .
  end
  command mv -i $argv
end
