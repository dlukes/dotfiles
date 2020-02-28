function cp --wraps cp
  # if only one argument is provided, default to copying into $PWD
  if test (count $argv) -eq 1
    set -a argv .
  end
  command cp -i $argv
end
