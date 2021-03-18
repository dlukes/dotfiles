function unarchive --description 'Extract archive'
  for path in $argv
    unarchive_one $path
  end
end

function unarchive_one
  set -l split (string split -rm1 . $argv)
  set -l cmd

  if string match -qr '\.tar$' $split[1]; or string match -qr '^t' $split[2]
    set -l compression
    switch (string replace -r '^t' '' $split[2])
      case gz
        set compression z
      case bz2
        set compression j
      case xz
        set compression J
      case '*'
        echo >&2 "Don't know how to unarchive '$argv'"
        return
    end
    set cmd tar x{$compression}f

  else if string match -qr '^gz$' $split[2]
    set cmd gunzip
  else if string match -qr '^bz2$' $split[2]
    set cmd bunzip2
  else if string match -qr '^xz$' $split[2]
    set cmd unxz
  else if string match -qr '^zip$' $split[2]
    set cmd unzip
  else if string match -qr '^7z$' $split[2]
    set cmd 7z x
  else
    echo >&2 "Don't know how to unarchive '$argv'"
    return
  end

  set -l tmp (mktemp -d)
  set -l old_pwd (pwd)
  set -l bname (basename $argv)
  set -l dname
  set -a cmd $bname

  mv -t $tmp $argv; or return
  cd $tmp
  $cmd
  set -l cmd_status $status
  mv -t $old_pwd $bname
  test $cmd_status = 0; or return

  set -l contents (find . -maxdepth 1 -print0 | string split0)
  if test (count $contents) = 1 -a -d $contents[1]
    set dname *
    mv -t $old_pwd $dname
  else
    set dname (string replace -r '.tar$' '' $split[1])
    set -l dpath $old_pwd/$dname
    mkdir $dpath
    mv -t $dpath *
  end

  cd $old_pwd
  echo >&2 "Unarchived '$argv' to '$dname' using command: '$cmd'"
end

# test:
# tar czf functions.tgz *.fish
# unarchive foo.tar.gz foo.tgz foo.tar.bz2 foo.tbz2 foo.zip bar.tar.xx bar.txx baz functions.tgz
