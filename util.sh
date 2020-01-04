function is_macos {
  [[ $( uname ) == Darwin ]]
}

function brew_install_or_upgrade {
  if brew ls --versions $1 >/dev/null; then
    brew upgrade $1
  else
    brew install $1
  fi
}

# vim: set ft=zsh:
