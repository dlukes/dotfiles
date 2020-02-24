#!/bin/sh

emacs=$( dirname $( realpath "$0" ) )

# wrapper script and desktop file
sudo cp "$emacs/spacemacs.sh" /usr/bin
sudo cp "$emacs/spacemacs.desktop" /usr/share/applications/

cd "$( mktemp -d )"
curl -O https://raw.githubusercontent.com/syl20bnr/spacemacs/master/assets/spacemacs.svg

# SVG
sudo find /usr/share -name emacs.svg -exec cp spacemacs.svg {} \;
sudo find /usr/share -name emacs\?\?.svg  -exec cp spacemacs.svg {} \;

# PNG
sizes=(16x16 24x24 32x32 48x48 64x64 128x128)
for size in $sizes; do
  convert -background none -resize $size spacemacs.svg spacemacs.png
  sudo find /usr/share -iname \*emacs\*.png -path \*$size\* -exec cp spacemacs.png {} \;
done
