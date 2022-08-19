#!/bin/sh

set -euf
script_dir=$(dirname "$(realpath "$0")")
. "$script_dir"/../misc/util.sh

if ! is_macos; then
  >&2 echo '>>> Applying system-wide config tweaks.'
  >&2 echo '>>> Modified config files can be listed via rpm -Vac.'

  # NOTE: If SELinux gives you grief, try restorecon on the modified file?

  # Tweak sudo config so that it behaves as much as possible as my regular environment
  # when the -E option is passed. See also sudoe and sudof abbrs, and comments in
  # sudo.fish, which was a very bad idea and is now disabled. For the tweaks applied
  # here, see:
  #
  #   - https://unix.stackexchange.com/a/91572
  #   - https://superuser.com/a/927599
  #
  # An interesting avenue was setting SUDO_EDITOR to a custom program and just running
  # visudo, cf. here: https://unix.stackexchange.com/a/673247. However, it turned out to
  # be a dead end because visudo tends to ship compiled without --enable-env-editor, so
  # that it disregards this variable if it's not one of a small set of known values.
  sudoers=/etc/sudoers
  sudoers_tmp=$sudoers.tmp
  sudo cat $sudoers |
    sed -E \
      -e 's/^(Defaults.*always_set_home)/# \1/' \
      -e 's/^(Defaults.*env_(reset|keep))/# \1/' \
      -e 's/^(Defaults.*secure_path)/# \1/' |
    sudo tee $sudoers_tmp >/dev/null
  sudo visudo -c $sudoers_tmp
  sudo chmod 440 $sudoers_tmp
  sudo mv $sudoers_tmp $sudoers
fi

