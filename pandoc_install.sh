#!/usr/bin/env zsh

set -e
source ${0:a:h}/util.sh

if is_macos; then
  >&2 echo "We're on macOS, so installing with brew..."
  brew_install_or_upgrade pandoc
  brew_install_or_upgrade pandoc-crossref
  exit
fi

tmpdir=`mktemp -d`
cd $tmpdir

>&2 echo "Getting download links and archive names..."
pandoc_link=$(
  curl -sL https://github.com/jgm/pandoc/releases/latest |
    grep -oPm 1 '/jgm/.*?/pandoc.*?-linux-amd64\.tar\.gz'
           )
crossref_link=$(
  curl -sL https://github.com/lierdakil/pandoc-crossref/releases/latest |
    grep -oPm 1 '/lierdakil/.*?/linux.*?\.tar\.gz'
             )
pandoc_archive_name=`basename $pandoc_link`
crossref_archive_name=`basename $crossref_link`

>&2 echo "Fetching and extracting archives..."
wget -q https://github.com$pandoc_link
wget -q https://github.com$crossref_link
tar xzf $pandoc_archive_name
tar xzf $crossref_archive_name

>&2 echo "Removing archives..."
rm -f $pandoc_archive_name
rm -f $crossref_archive_name

>&2 echo "Merging pandoc-crossref into pandoc's directory tree..."
mv pandoc-crossref pandoc-*/bin
mv pandoc-crossref.1 pandoc-*/share/man/man1

>&2 echo "Moving everything under '$HOME/.local...'"
local_tree=$HOME/.local
mkdir -p $local_tree/bin
mkdir -p $local_tree/share/man/man1
mv pandoc-* $local_tree

>&2 echo "Creating symlinks..."
cp -frs $local_tree/pandoc-*/* $local_tree/

>&2 echo "You should now be able to run pandoc, pandoc-citeproc and pandoc-crossref."
>&2 echo "If it doesn't work, make sure that:"
>&2 echo "  - '$local_tree/bin' is on your PATH"
>&2 echo "  - '$local_tree/share/man' is on your MANPATH"
