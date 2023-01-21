# Installation

Clone the repo as `~/.files`:

```bash
git clone git@github.com:dlukes/dotfiles.github ~/.files
```

Run `bin/symlink.sh` to create symlinks to the config files in appropriate places. If
the `DOTFILES_UNLINK` env var is set, the symlinks will instead be removed.

`bin/get` lists the various `*_install.sh` scripts available, `bin/get substring` will
run the first such script containing `substring` in its name.

# TODO

- Redefine Fish abbreviations for commands that can be run with or without a `sudo`
  prefix (Podman) with `--position anywhere` once v3.6.0 becomes more widely available:
  <https://github.com/fish-shell/fish-shell/releases/tag/3.6.0>
