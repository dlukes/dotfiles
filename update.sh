#!/usr/bin/env sh

git pull
git submodule update --remote
git submodule update --recursive
git commit -a
