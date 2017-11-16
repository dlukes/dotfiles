#!/usr/bin/env perl

use utf8;
use strict;
use warnings;
use FindBin;
use File::Basename;

system "git submodule update --init --recursive"
  and die "Couldn't update/init submodules: $!";

my $dotdir = $FindBin::RealBin.'/';
my $home = $ENV{'HOME'}.'/';
my @dotfiles = grep { !/
  ^.git(modules|ignore)?$
 |^\.config$
 |^\.{1,2}$
 |disabled
 |\.DS_Store
  /x } glob '.* texmf .config/*';

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

for my $snippet_dir (glob $dotdir."snippets/*") {
  print STDERR "Symlinking $snippet_dir to $dotdir.emacs.d/private/snippets/$snippet_dir.\n";
  symlink $snippet_dir, $dotdir.".emacs.d/private/snippets/".(basename $snippet_dir) or die "Symlinking failed: $!";
}
