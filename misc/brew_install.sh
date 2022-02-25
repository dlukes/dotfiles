#!/bin/sh

set -euf

# tidy-html5 is because macOS ships with an obsolete version of tidy, which other tools
# (like Doom Emacs) might silently pick up and use, corrupting HTML and XML files left
# and right.
brew install \
  fish git \
  coreutils findutils gnu-sed gnu-tar gawk \
  gcc readline openssl \
  tidy-html5
