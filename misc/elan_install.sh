#!/bin/sh

dot_local="$HOME/.local"

archive_url=$(
  curl -fsSL https://tla.mpi.nl/tools/tla-tools/elan/download/ |
    grep -oiPm1 'https?://.*?_linux\.tar\.gz' |
    head -1
)
archive_basename=$( basename "$archive_url" )

if [ -z "$archive_url" ]; then
  >&2 echo 'Unable to determine download URL.'
  exit 1
fi

# quit if this version is already installed
version=$( echo "$archive_basename" | cut -f2 -d_ | cut -f1,2 -d- )
major="${version%-*}"
minor="${version#*-}"
unpack_dir="$dot_local/ELAN-$major.$minor"
if [ -d "$unpack_dir" ]; then
  >&2 echo "The latest ELAN version $major.$minor is already installed, only updating desktop launcher."
else
  mkdir -p "$unpack_dir"
  cd "$dot_local"
  curl -fLO "$archive_url"
  tar xzf "$archive_basename" -C "$unpack_dir" --strip-components 1 &&
    rm -f "$archive_basename"
fi

# find the newest available ELAN executable
elan_bin=$(
  find "$dot_local" -type f -executable -name ELAN\* -printf '%T@\t%p\n' |
    sort -nr |
    head -n1 |
    cut -f2
)

launcher="$dot_local/share/applications/ELAN.desktop"
cat <<EOF >"$launcher"
[Desktop Entry]
Name=ELAN
Exec=env "_JAVA_OPTIONS=-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Xmx2048m" "$elan_bin" %U
Type=Application
Terminal=false
Categories=Office;
MimeType=text/xml
EOF
dbus-launch gio set "$launcher" metadata::trusted yes
