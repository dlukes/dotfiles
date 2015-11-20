#!/usr/bin/env perl

use utf8;
use strict;
use warnings;
use FindBin;

# system "git submodule foreach git submodule init";
# system "git submodule foreach git submodule update";

my $dotdir = $FindBin::RealBin.'/';
my $home = $ENV{'HOME'}.'/';
my @dotfiles = grep { !/
  ^.git(modules|ignore)?$
 |^\.{1,2}$
  /x } glob '.* texmf';

for my $dotfile (@dotfiles) {
  # check for pre-existing links/files/dirs
  if (-l $home.$dotfile) {
    unlink $home.$dotfile or die "Unable to remove existing symlink: $!";
  } elsif (-x $home.$dotfile) {
    print STDERR "WARNING: a non-symlink file named $dotfile already exists "
      ."in $home. Symlink not created.\n";
    next;
  }
  print STDERR "Symlinking $dotdir$dotfile to $home$dotfile.\n";
  symlink $dotdir.$dotfile, $home.$dotfile or die "Symlinking failed: $!";
}

# use Data::Dumper;
# print Dumper \@dotfiles;
# SYNOPSIS
#     setup.sh [-f]
#
# DESCRIPTION
# Set up symbolic links in $HOME for rc files contained in the same directory
# as this script (following any possible symlinks).
#
#     -f      Clobber any existing files in the home directory by the symlinks.

# if [[ "$OSTYPE" == darwin* ]]; then
#     LN=gln
# else
#     LN=ln
# fi
#
# dot="$HOME/Google Drive/dotfiles"
#
# cd
# for file in $dot/.* $dot/texmf $dot/.emacs-conf/.emacs $dot/.zprezto/runcoms/*; do
#     link=`basename ${file/runcoms\//runcoms\/.}`
#     echo -n "Adding symbolic link named $link to $file to home directory... "
#     if [[ ! -f "$HOME/$file" ]]; then
#         $LN -sf $file $link && echo OK || echo "Error creating link."
#     elif [[ $1 == -f ]]; then
#         $LN -sf $file $link && echo "Existing file replaced by link." || echo "Error creating link."
#     else
#         echo "Runcom already exists and is a file, not a symlink. Use -f to overwrite it with symlink."
#     fi
# done
