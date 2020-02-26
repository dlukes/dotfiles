function fish_right_prompt --description 'Write out the right prompt'
  # save last status; NOTE: this *has* to stay first, otherwise the
  # status of the last interactively run command will be overwritten by
  # the previous function-internal command
  set last_pipestatus $pipestatus
  set sep (set_color normal)' •'

  if test $CMD_DURATION -gt 3000
    set duration '⏱  ' (set_color yellow)(date -ud @(math $CMD_DURATION / 1000) +%T)$sep
  end

  set prompt_status (__fish_print_pipestatus ' ⚡' $sep '|' \
    (set_color $fish_color_status) \
    (set_color --bold $fish_color_status) \
    $last_pipestatus)

  # check VCS info at most every .5s
  set now (date +%s%N)
  if not set -q _last_vcs_check || test (math $now - $_last_vcs_check) -gt 500000000
    set -g _vcs (fish_vcs_prompt)
  end
  set -g _last_vcs_check (date +%s%N)

  echo -ns $duration $prompt_status $_vcs
end
