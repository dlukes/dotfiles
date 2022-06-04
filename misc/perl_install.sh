#!/bin/sh

set -euf
script_dir=$(dirname "$(realpath "$0")")
. "$script_dir"/util.sh



# --------------------------------------------------------------------------------- CPAN {{{1


if ! command -v cpan >/dev/null 2>&1; then
  >&2 echo '>>> CPAN not found, but it is a requirement for local::lib.'
  if is_macos; then
    >&2 echo 'Installing Perl including CPAN with brew.'
    brew_install_or_upgrade perl
  elif is_fedora; then
    >&2 echo '>>> Installing CPAN with dnf.'
    sudo dnf in -by perl-CPAN
  else
    >&2 echo ">>> Don't know how to install CPAN in this context, aborting."
    exit 1
  fi
fi



# --------------------------------------------------------------------------- local::lib {{{1


ll="$HOME"/.local/perl5

if ! [ -d "$ll" ]; then
  cd "$(mktemp -d)"
  ll_url=$(
    curl -sfL https://metacpan.org/pod/local::lib |
      grep -oPm1 'https://.*?/local-lib-.*?\.tar\.gz'
  )
  curl -sSfLO "$ll_url"
  ll_tar=$(basename "$ll_url")
  tar xzf "$ll_tar"
  ll_dir="${ll_tar%.tar.gz}"
  cd "$ll_dir"
  perl Makefile.PL --bootstrap="$ll"
  make test
  make install
fi

eval "$(perl -I"$ll"/lib/perl5 -Mlocal::lib="--shelltype=bourne,$ll")"



# -------------------------------------------------------------------------------- cpanm {{{1


if ! command -v cpanm >/dev/null 2>&1; then
  curl -sSfL https://cpanmin.us | perl - App::cpanminus
fi
cpanm --self-upgrade



# --------------------------------------------------------------------- Perl-based tools {{{1


# Tom Christiansen's Unicode-related tools (uniprops, unichars...), maintained by Brian
# D. Foy.
cpanm Unicode::Tussle
