#!/usr/bin/env sh

git submodule foreach git submodule init
git submodule foreach git submodule update
