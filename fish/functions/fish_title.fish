function fish_title
  # if in tmux, sets the active pane title...
  if string match -rq 'screen|tmux' $TERM
    prompt_pwd
  # ... otherwise, the terminal title
  else
    echo (whoami)@(hostname): (prompt_pwd)
  end

  # tmux notes:
  #
  # - unlike other shells where you have to use raw escape sequences to
  #   achieve this (search for #T and NAMES AND TITLES in man tmux for
  #   more info), in fish you can just tweak fish_title to print the
  #   string you want
  # - in .tmux.conf, the tmux window title is configured to include the
  #   title of the active pane
end
