#!/bin/sh

# Stuff you might want to pull in on every Fedora install to make your life easier.

sudo dnf groupinstall "Development Tools" "Development Libraries"
sudo dnf install libstdc++-devel
