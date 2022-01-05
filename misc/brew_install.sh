#!/bin/sh

set -euf

brew install \
  fish git \
  coreutils findutils gnu-sed gnu-tar gawk \
  gcc readline openssl
