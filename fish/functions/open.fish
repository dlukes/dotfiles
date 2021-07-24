function open
  if type -q xdg-open
    xdg-open $argv
  else
    command open $argv
  end
end
