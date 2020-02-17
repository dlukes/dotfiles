function fish_user_key_bindings
  set fzf ~/.local/fzf/shell/key-bindings.fish
  test -f $fzf
    and source $fzf
    and fzf_key_bindings
end
