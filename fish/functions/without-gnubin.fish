function without-gnubin
  if not set -q argv[1]
    echo >&2 'On macOS, prepend this when retrying a failed compilation command, in order to bypass the GNU toolchain.'
    return
  end

  set -l --path path
  for p in $PATH
    if not string match -r '/gnubin$' $p >/dev/null
      set -a path $p
    end
  end
  env PATH="$path" $argv
end
