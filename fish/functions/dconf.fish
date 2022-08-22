# A wrapper for the dconf CLI tool which allows manipulating (a copy of) a specific
# dconf db file. Pass it in as an argument prepended with @, e.g.:
#
#   dconf dump / @/usr/share/gdm/greeter-dconf-defaults
function dconf --wraps dconf
  set -l db_path (string match -r -- '^@.*' $argv)
  switch (count $db_path)
    case 0
      command dconf $argv
    case 1
      set -e argv[(contains -i $db_path $argv)]
      set -l db_path (string sub -s2 -- $db_path)
      set -l tmpdir (mktemp -d)
      mkdir $tmpdir/dconf
      cp -a $db_path $tmpdir/dconf/user
        or echo >&2 "No dconf db found at path: '$db_path'" && return 1
      XDG_CONFIG_HOME=$tmpdir command dconf $argv
    case '*'
      echo >&2 'Only one dconf db path can be specified via an @-argument!'
      return 1
  end
end
