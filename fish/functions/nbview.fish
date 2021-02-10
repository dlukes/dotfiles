function nbview --description 'View an .ipynb file at the command line'
  jupytext $argv --to markdown -o - | bat -l md
end
