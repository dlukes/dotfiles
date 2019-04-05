#!/usr/bin/env zsh

installer_url=$(
  curl -s https://tla.mpi.nl/tools/tla-tools/elan/download/ |
    grep -oPm1 'https?://.*?linux\.bin'
)
installer_basename=${installer_url:t}

if [[ -z $installer_url ]]; then
  >&2 echo "Unable to determine download URL."
  exit 1
fi

# quit if this version is already installed
version=$( echo $installer_basename | cut -f2 -d_ )
major=${version%-*}
minor=${version#*-}
install_dir=$HOME/ELAN_$major.$minor
if [[ -d $install_dir ]]; then
  >&2 echo "The latest ELAN version $major.$minor is already installed, aborting."
  exit 0
fi

temp_dir=$( mktemp -d )
cd $temp_dir

wget $installer_url
sh $installer_basename

# find the newest available ELAN executable
elan_bin=$(
  find $HOME -not -path '*/\.*' -type f -executable -name 'ELAN_*' -printf '%T@\t%p\n' |
    grep -P '/ELAN_\d+\.\d+$' |
    sort -nr |
    head -n1 |
    cut -f2
)

cat <<EOF >~/.local/share/applications/ELAN.desktop
[Desktop Entry]
Name=ELAN
Exec=env "_JAVA_OPTIONS=-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Xmx2048m" "$elan_bin" %U
Type=Application
Terminal=false
Categories=Office;
MimeType=text/xml
EOF

>&2 echo "If you haven't used the .desktop launcher before, register it by double clicking it from Files."
