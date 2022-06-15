#!/bin/sh

set -euf
script_dir=$(dirname "$(realpath "$0")")
. "$script_dir"/util.sh

if is_macos; then
  >&2 echo "On macOS, install manually from <https://www.fon.hum.uva.nl/praat/download_mac.html>."
  exit 1
fi

>&2 echo "Current version: $(praat --version 2>/dev/null || echo none)"

prefix="$HOME"/.local
base_url=https://www.fon.hum.uva.nl/praat
latest_version_archive=$(
  curlk -sSfL "$base_url"/download_linux.html |
    grep -oPm1 'praat.*?_linux64.tar.gz' |
    head -1
)
download_url="$base_url/$latest_version_archive"

cd "$prefix"/bin
curlk -sSfLO "$download_url"
tar xzf "$latest_version_archive"
rm "$latest_version_archive"

cat <<'EOF' >"$prefix"/share/applications/praat.desktop
[Desktop Entry]
Name=Praat
GenericName=Sound Editor
Comment=Doing phonetics by computer
Icon=audacity
Type=Application
Categories=AudioVideo;Audio;AudioVideoEditing;
# %F or %U?
Exec=praat --hide-picture --open %F
StartupNotify=false
Terminal=false
# MimeType=application/x-audacity-project;audio/flac;audio/x-flac;audio/mpeg;application/ogg;audio/x-vorbis+ogg;
EOF

>&2 echo "Newly installed version: $(praat --version)"
