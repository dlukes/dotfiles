#!/bin/sh

set -eu
script_dir=$(dirname "$(realpath "$0")")
cd "$script_dir"



# ------------------------------------------------------------------------ User services {{{1


for unit in "$PWD"/user/*; do
  systemctl --user enable --force --now "$unit"
done



# ---------------------------------------------------------------------- System services {{{1


for unit in "$PWD"/system/*; do
  # The appropriate SELinux context can be figured out by running ls -Z on a sample
  # distro-provided Systemd unit file.
  chcon system_u:object_r:systemd_unit_file_t:s0 "$unit"
  sudo systemctl enable --force --now "$unit"
done
