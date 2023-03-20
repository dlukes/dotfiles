#!/bin/bash

set -eufo pipefail
cd "$HOME"/.local/share
repo=python-type-stubs
if [ -d "$repo" ]; then
  cd "$repo"
  dirty=$(git status --porcelain)
  if [ -n "$dirty" ]; then
    git stash
  fi
  git pull
  if [ -n "$dirty" ]; then
    git stash pop
  fi
else
  git clone git@github.com:microsoft/"$repo".git
fi
