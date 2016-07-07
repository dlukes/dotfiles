#!/usr/bin/env zsh

# wrapper script and desktop file
sudo cp spacemacs.sh /usr/local/bin
sudo cp spacemacs.desktop /usr/share/applications/

cd $TMPDIR
curl -O https://raw.githubusercontent.com/syl20bnr/spacemacs/master/assets/spacemacs.svg

# SVG
find /usr/share -name emacs.svg -o -name emacs\?\?.svg  -exec sudo cp spacemacs.svg {} \;

# PNG
sizes=(16x16 24x24 32x32 48x48 128x128)
for size in $sizes; do
  convert -background none -resize $size spacemacs.svg spacemacs.png
  find /usr/share -iname \*emacs\*.png -path \*$size\* -exec sudo cp spacemacs.png {} \;
done
