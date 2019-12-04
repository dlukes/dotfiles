#!/usr/bin/env zsh

# Consider downloading pyenv and setting Python up that way, so that
# it's consistent across machines.

# on macOS, I have brew, so no --user; if I ever end up configuring
# pyenv across the board (cf. above), remove --user everywhere
if ! uname | grep -q Darwin; then
  user=--user
fi

pip3 install $user --upgrade --upgrade-strategy eager \
  ipython \
  ipdb \
  \
  lxml \
  regex \
  requests \
  dill \
  \
  numpy \
  pandas \
  xlrd \
  \
  matplotlib \
  seaborn \
  \
  corpy \
  \
  httpie \
  \
  'python-language-server[rope,pyflakes]' \
  pyls-mypy \
  pyls-black \
  \
  pip \
  'poetry>=1.0.0a0' \
  pipenv
