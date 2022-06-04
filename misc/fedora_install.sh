#!/bin/sh

# Stuff you might want to pull in on every Fedora install to make your life easier.

sudo dnf grp install -by "Development Tools" "Development Libraries"
sudo dnf in -by libstdc++-devel \
  gnome-shell-extension-appindicator
