# Installation

Clone the repo as `~/.files`:

```sh
git clone --recursive git@github.com:dlukes/dotfiles.github ~/.files
```

The run the various `bin/*install.sh` scripts as required, and
`bin/symlink.sh` to create symlinks to the config files in appropriate
places.

If the `DOTFILES_UNLINK` env var is set, the symlinks will instead be
removed.
