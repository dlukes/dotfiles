#!/usr/bin/env perl

use utf8;
use strict;
use warnings;
use FindBin;

# perhaps all of this should just be git submodule update --init --recursive,
# at least that's the way the Prezto folks are recommending to do updates
# system "git submodule init"
#     and die "Couldn't init submodules: $!";
# system "git submodule update"
#     and die "Couldn't update submodules: $!";
# system "git submodule foreach git submodule init"
#     and die "Couldn't init submodule submodules: $!";
# system "git submodule foreach git submodule update"
#     and die "Couldn't update submodule submodules: $!";

my $dotdir = $FindBin::RealBin.'/';
my $home = $ENV{'HOME'}.'/';
my @dotfiles = grep { !/
  ^.git(modules|ignore)?$
 |^\.{1,2}$
 |disabled
 |\.DS_Store
  /x } glob '.* texmf';

for my $dotfile (@dotfiles) {
  # check for pre-existing links/files/dirs
  if (-l $home.$dotfile) {
    unlink $home.$dotfile or die "Unable to remove existing symlink: $!";
  } elsif (-e $home.$dotfile) {
    print STDERR "WARNING: a non-symlink file named $dotfile already exists "
      ."in $home. Symlink not created.\n";
    next;
  }
  print STDERR "Symlinking $dotdir$dotfile to $home$dotfile.\n";
  symlink $dotdir.$dotfile, $home.$dotfile or die "Symlinking failed: $!";
}
