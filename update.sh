#!/usr/bin/env sh

git pull
# first do a recursive update of all submodules to pull in changes approved by third-party repositories
git submodule update --recursive
# then update toplevel submodules to the newest remote version
git submodule update --remote
git commit -am "update submodules"
