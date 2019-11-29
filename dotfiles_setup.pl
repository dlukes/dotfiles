#!/usr/bin/env perl

use utf8;
use strict;
use warnings;
use FindBin;
use File::Basename;
use File::Path;

system "git submodule update --init --recursive"
  and die "Couldn't update/init submodules: $!";

my $dotdir = $FindBin::RealBin.'/';
my $home = $ENV{'HOME'}.'/';
my @dotfiles = grep { !/
  \/.git(modules|ignore)?$
 |\/\.{1,2}$
 |disabled
 |\.DS_Store
 |\.mypy_cache
 |\.config$
  /x } glob $dotdir.'{.*,texmf}';

create_symlinks($dotdir, $home, @dotfiles);
create_symlinks($dotdir.'.config/', $home.'.config/', glob $dotdir.'.config/*');
create_symlinks($dotdir, $home.'.config/nvim/', $dotdir.'init.vim');
create_symlinks($dotdir.'snippets/', $dotdir.'.emacs.d/private/snippets/', glob $dotdir.'snippets/*');

sub create_symlinks {
  my $from = shift;
  my $to = shift;
  my @targets = @_;
  mkdir $to;
  for my $target (@targets) {
    $target = basename $target;
    # check for pre-existing links/files/dirs
    if (-l $to.$target) {
      unlink $to.$target or die "Unable to remove existing symlink: $!";
    } elsif (-e $to.$target) {
      print STDERR "WARNING: a non-symlink file named $target already exists "
          ."in $to. Symlink not created.\n";
      next;
    }
    symlink $from.$target, $to.$target or die "Symlinking failed: $!";
    print STDERR "Created symlink $to$target to $from$target.\n";
  }
}
