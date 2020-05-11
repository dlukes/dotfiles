function pip-uninstall-by-date
  set -l func_name (status function)
  set -l help "\
NAME
    $func_name -- Uninstall Python packages by date

SYNOPSIS
    $func_name [OPTIONS]

DESCRIPTION

    pip currently doesn't make a distinction between packages installed
    explicitly vs. those installed as dependencies, so uninstalling
    a package you just wanted to try out can leave junk behind. This
    function uninstalls all packages added within a time window you can
    specify based on when the experimentation session you want to roll
    back happened.

OPTIONS
    -h/--help: Show this help message
    -f/--from DATE: Start date
    -t/--to DATE: End date

EXAMPLES
    $func_name --from '-3 hours' --to '-2 hours'
"

  argparse "h/help" "f/from=" "t/to=" -- $argv
  or echo >&2 -n $help && return 1
  if set -q _flag_help
    echo -n $help
    return
  end
  if set -q _flag_from
    set from -newermt $_flag_from
  end
  if set -q _flag_to
    set to -not -newermt $_flag_to
  end

  set -l site_package_dirs (python3 -c "
import site
for sp in site.getsitepackages():
  print(sp)
print(site.getusersitepackages())
  ")
  set -l packages
  set -l mtimes
  for sp_dir in $site_package_dirs
    if test -d $sp_dir
      find $sp_dir $from $to \( -name \*.dist-info -o -name \*.egg-info \) -printf "%TY-%Tm-%Td+%TH:%TM %p\n" |
        sort |
        while read -l mtime_infodir
          set mtime_infodir (string split --max 1 " " $mtime_infodir)
          set -l package (string replace -r -- "-.*" "" (basename $mtime_infodir[2]))""
          if test $package != pip
            set -a packages $package
            set -a mtimes $mtime_infodir[1]
          end
        end
    end
  end

  if ! set -q packages[1]
    echo >&2 "No packages found for specified time range."
    return
  end

  echo >&2 "Will uninstall the following packages:"
  set -l last_mtime ""
  for i in (seq 1 (count $packages))
    set -l mtime $mtimes[$i]
    if test $mtime != $last_mtime
      echo >&2 "- installed on $mtime:"
      set last_mtime $mtime
    end
    echo >&2 "  - $packages[$i]"
  end
  read -lP "Proceed? [y/N]" proceed
  switch $proceed
    case Y y
      pip uninstall --yes $packages
    case "" N n
      return
  end
end
