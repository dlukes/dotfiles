#!/bin/sh

# Stuff you might want to pull in on every Fedora install to make your life easier.

sudo dnf grp install -by "Development Tools" "Development Libraries"
sudo dnf in -by libstdc++-devel \
  gnome-shell-extension-appindicator \
  gnome-shell-extension-caffeine \
  gnome-shell-extension-freon \
  gnome-shell-extension-gamemode \
  gnome-shell-extension-openweather \
  gnome-shell-extension-sound-output-device-chooser \
  gnome-shell-extension-system-monitor-applet

# Suggestions:
#
# - gnome-shell-extension-pop-shell if you want a more sophisticated tiling window
#   environment (you probably don't, you're used to GNOME Shell as it is)
# - gnome-shell-extension-netspeed for a dedicated network speed indicator, but the
#   system monitor applet has this too and it looks less distracting
