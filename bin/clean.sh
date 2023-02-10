#!/bin/bash

# A collection of commands to run in order to clean up storage space.

script_dir=$(dirname "$(realpath "$0")")
source "$script_dir"/../misc/util.sh

run() {
  case "$1" in
    sudo)
      local cmd="$2"
      ;;
    *)
      local cmd="$1"
      ;;
  esac

  if command -v "$cmd" >/dev/null; then
    info "Running: $*"
    eval "$@"
  else
    warning "Command $cmd not found, can't run: $*"
  fi
}

run mamba clean --all
run cargo cache -e
run podman system prune
run sudo podman system prune
run sudo journalctl --flush --rotate
run sudo journalctl --vacuum-size=100M  # --vacuum-time=1day, --vacuum-files=5
run rm -rf ~/.cache/{pycache,pip}
run sudo apt autoremove
run sudo apt clean
run sudo dnf autoremove
run sudo dnf clean all

info "\
Done. To detect more cleanup opportunities, consider running:

  br -w path/to/dir
  podman system df
  sudo podman system df"
