#!/bin/sh

csl="$HOME/.local/share/csl"
styles=(
  apa
  chicago-author-date
  iso690-author-date-cs
  unified-style-sheet-for-linguistics
)

mkdir -p "$csl"
cd "$csl"
for style in "${styles[@]}"; do
  >&2 echo "Downloading style $style from zotero.org..."
  curl -sLOJ https://www.zotero.org/styles/$style
done
