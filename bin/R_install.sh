#!/bin/sh

check_install_R_package() {
  R --quiet -e "require('$1') || install.packages('$1', repos = 'https://cloud.r-project.org')"
}

check_install_R_package languageserver
check_install_R_package tidyverse
