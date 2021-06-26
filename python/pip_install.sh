#!/bin/sh

pip3 install --upgrade --upgrade-strategy eager \
  pip \
  wheel
pip3 install --upgrade --upgrade-strategy eager \
  ipython \
  ipdb \
  scalene \
  poetry \
  pdm \
  rich \
  \
  lxml \
  regex \
  requests \
  \
  jupyterlab \
  pandas \
  openpyxl \
  scikit-learn \
  statsmodels \
  altair \
  matplotlib \
  \
  httpie \
  bpytop \
  glances \
  \
  jedi-language-server \
  pylint \
  pylint-venv
