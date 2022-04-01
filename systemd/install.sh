#!/bin/sh

# NOTE: With SELinux enabled, symlinking system-wide config files to your home dir is...
# tricky. It might sometimes work if you chcon appropriately, but sometimes even that is
# not enough and the system process won't be allowed to read your home dir. So better
# just copy them. At least, it looks like you don't have to futz around with chcon, yay.

set -eu
script_dir=$(dirname "$(realpath "$0")")
cd "$script_dir"

log() {
  { printf -- "-%.s" $(seq $(tput cols)); echo; } >&2
  >&2 echo Configuring "$1"
  { printf -- "-%.s" $(seq $(tput cols)); echo; } >&2
}



# ---------------------------------------------------------------------- Journald config {{{1


journald_conf_d=/etc/systemd/journald.conf.d
log "$journald_conf_d"
sudo mkdir -p "$journald_conf_d"
sudo cp -t "$journald_conf_d" journald.conf.d/*
sudo systemctl restart systemd-journald



# ------------------------------------------------------------------------ User services {{{1


for unit in "$PWD"/user/*; do
  log "$unit"
  systemctl --no-block --user enable --force "$unit"
  systemctl --no-block --user restart "$(basename "$unit")"
done



# ---------------------------------------------------------------------- System services {{{1


for unit in "$PWD"/system/*; do
  log "$unit"
  sudo cp -t /etc/systemd/system "$unit"
  unit=$(basename "$unit")
  sudo systemctl --no-block enable --force $unit
  sudo systemctl --no-block restart $unit
done
