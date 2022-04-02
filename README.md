# Installation

Clone the repo as `~/.files`:

```sh
git clone git@github.com:dlukes/dotfiles.github ~/.files
```

Run `bin/symlink.sh` to create symlinks to the config files in appropriate places. If
the `DOTFILES_UNLINK` env var is set, the symlinks will instead be removed.

`bin/get` lists the various `*_install.sh` scripts available, `bin/get substring` will
run the first such script containing `substring` in its name.
