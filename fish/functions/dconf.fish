# A wrapper for the dconf CLI tool which allows manipulating (a copy of) a specific
# dconf db file. Usage:
#
#   DCONF_DB_PATH=/usr/share/gdm/greeter-dconf-defaults dconf dump /
function dconf --wraps dconf
  set -l src_path (status filename)
  if set -q DCONF_DB_PATH
    set -l tmpdir (mktemp -d)
    mkdir $tmpdir/dconf
    cp -a $DCONF_DB_PATH $tmpdir/dconf/user
      or echo >&2 "$src_path: No dconf db found at path: '$DCONF_DB_PATH'" && return 1
    XDG_CONFIG_HOME=$tmpdir command dconf $argv
  else
    command dconf $argv
  end
end
