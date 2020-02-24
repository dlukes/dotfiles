function ls --wraps exa
  exa --group-directories-first --sort newest $argv
end
