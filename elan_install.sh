#!/usr/bin/env zsh

dot_local=$HOME/.local

archive_url=$(
  curl -s https://tla.mpi.nl/tools/tla-tools/elan/download/ |
    grep -oiPm1 'https?://.*?_linux\.tar\.gz' |
    head -n1
)
archive_basename=${archive_url:t}

if [[ -z $archive_url ]]; then
  >&2 echo "Unable to determine download URL."
  exit 1
fi

# quit if this version is already installed
version=$( echo $archive_basename | cut -f2 -d_ | cut -f1,2 -d- )
major=${version%-*}
minor=${version#*-}
unpack_dir=$dot_local/ELAN-$major.$minor
if [[ -d $unpack_dir ]]; then
  >&2 echo "The latest ELAN version $major.$minor is already installed, only updating desktop launcher."
else
  mkdir -p $unpack_dir
  cd $dot_local
  wget $archive_url
  tar xvzf $archive_basename -C $unpack_dir --strip-components 1 && rm -f $archive_basename
fi

# find the newest available ELAN executable
elan_bin=$(
  find $dot_local -type f -executable -name ELAN -printf '%T@\t%p\n' |
    sort -nr |
    head -n1 |
    cut -f2
)

cat <<EOF >$dot_local/share/applications/ELAN.desktop
[Desktop Entry]
Name=ELAN
Exec=env "_JAVA_OPTIONS=-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Xmx2048m" "$elan_bin" %U
Type=Application
Terminal=false
Categories=Office;
MimeType=text/xml
EOF

>&2 echo "If you haven't used the .desktop launcher before, register it by double clicking it from Files."
