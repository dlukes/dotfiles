#!/bin/sh

set -euf
script_dir=$(dirname "$(realpath "$0")")
. "$script_dir/../misc/util.sh"

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
