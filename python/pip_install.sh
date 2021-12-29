#!/bin/sh

set -e
dirname=$( dirname "$0" )
. "$dirname/util.sh"

if command -v brew >/dev/null 2>&1; then
  brew_install_or_upgrade openblas
  brew_install_or_upgrade libjpeg
  export OPENBLAS=$(brew --prefix openblas)
fi

without_gnubin pip3 install --upgrade --upgrade-strategy eager \
  pip \
  wheel
without_gnubin pip3 install --upgrade --upgrade-strategy eager \
  pynvim \
  ipython \
  ipdb \
  black \
  rich \
  poetry \
  bpytop \
  scalene \
  py-spy \
  \
  lxml \
  regex \
  httpx \
  \
  jupyterlab \
  jupytext \
  pandas \
  openpyxl \
  scikit-learn \
  statsmodels \
  altair \
  matplotlib \
  seaborn
