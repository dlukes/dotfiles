function fish_right_prompt --description 'Write out the right prompt'
  # save last status; NOTE: this *has* to stay first, otherwise the
  # status of the last interactively run command will be overwritten by
  # the previous function-internal command
  set last_pipestatus $pipestatus

  if test $CMD_DURATION -gt 3000
    set duration '⏰ '(set_color yellow)(date -ud @(math $CMD_DURATION / 1000) +%T)
  end

  # NOTE: If status reporting stops working, the culprit is probably a fish_right_prompt
  # wrapper that fails to preserve $pipestatus, e.g. from Conda. Check if `type
  # fish_right_prompt` shows the present function, and if not, figure out how to get rid
  # of the wrapper, or make it preserve $pipestatus.
  #
  # By default, __fisht_print_pipestatus only prints if the *last* status was an error
  # (= anything other than code 0 or 141, see `type __fish_print_pipestatus`). However,
  # I'd rather see a warning whenever *any* status in the last pipe was an error. This
  # can be achieved by explicitly setting __fish_last_status and exporting it, see:
  # https://github.com/fish-shell/fish-shell/blob/5197bf75cde5934ebd5b44a97176effb03e89fd9/share/functions/fish_prompt.fish#L6
  set errors (string match -rv '^(?:0|141)$' -- $last_pipestatus)
  if test -n "$errors"
    set -x __fish_last_status $errors[1]
  end
  set prompt_status (__fish_print_pipestatus '⚡[' ']' '|' \
    (set_color $fish_color_status) \
    (set_color --bold $fish_color_status) \
    $last_pipestatus)

  # check VCS info at most every .5s
  set now (date +%s%N)
  if not set -q _last_vcs_check || test (math $now - $_last_vcs_check) -gt 500000000
    set -g _vcs (string trim (fish_vcs_prompt))
  end
  set -g _last_vcs_check (date +%s%N)

  string join (set_color normal)' • ' $duration $prompt_status $_vcs
end
