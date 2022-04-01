#!/bin/sh

set -eu
script_dir=$(dirname "$(realpath "$0")")
cd "$script_dir"

log() {
  { printf -- "-%.s" $(seq $(tput cols)); echo; } >&2
  >&2 echo Configuring unit file "$1"
  { printf -- "-%.s" $(seq $(tput cols)); echo; } >&2
}



# ------------------------------------------------------------------------ User services {{{1


for unit in "$PWD"/user/*; do
  log "$unit"
  systemctl --no-block --user enable --force "$unit"
  systemctl --no-block --user restart "$(basename "$unit")"
done



# ---------------------------------------------------------------------- System services {{{1


for unit in "$PWD"/system/*; do
  log "$unit"
  # The appropriate SELinux context can be figured out by running ls -Z on a sample
  # distro-provided Systemd unit file.
  chcon system_u:object_r:systemd_unit_file_t:s0 "$unit"
  sudo systemctl --no-block enable --force "$unit"
  sudo systemctl --no-block restart "$(basename "$unit")"
done
