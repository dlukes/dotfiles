#!/bin/bash

set -eufo pipefail
cd "$HOME"/.local/share
repo=python-type-stubs
if [ -d "$repo" ]; then
  cd "$repo"
  git pull
else
  git clone git@github.com:microsoft/"$repo".git
fi
