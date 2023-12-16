function ls --wraps eza
  eza --group-directories-first --sort newest --icons $argv
end
