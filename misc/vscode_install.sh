#!/bin/sh

# don't set -e before the next line, failure is ok there, it indicates
# VSCode is not installed yet on this machine
current_version=$( code --version 2>/dev/null | head -n1 )
set -e
newest_version=$(
  curl -sSL https://code.visualstudio.com/updates/ |
    grep -oP '(version|Update) \d+\.\d+(\.\d+)?' |
    cut -f2 -d' ' |
    tail -n1
)
if [ -n "$current_version" ] && [ "$current_version" == "$newest_version" ]; then
  >&2 echo "Visual Studio Code is up to date (current version: $current_version, newest version: $newest_version)."
  exit
fi

>&2 echo "Removing old Visual Studio Code version $current_version..."
cd ~/.local
rm -rf VSCode-linux-x64

>&2 echo "Downloading new Visual Studio Code version $newest_version..."
curl -sSLo vscode.tar.gz 'https://update.code.visualstudio.com/latest/linux-x64/stable'

>&2 echo "Extracting archive..."
tar xzf vscode.tar.gz
rm vscode.tar.gz

>&2 echo "Setting up symlinks and desktop files..."
cd bin
ln -sf ../VSCode-linux-x64/bin/code
launcher="$HOME/.local/share/applications/code.desktop"
cat <<'EOF' >
[Desktop Entry]
Name=Visual Studio Code
Comment=Visual Studio Code
GenericName=Text Editor
Exec=/home/david/.local/bin/code %U
Icon=visual-studio-code
Type=Application
Terminal=false
StartupNotify=true
StartupWMClass=Code
Categories=Development;TextEditor;
MimeType=text/plain;
EOF
dbus-launch gio set "$launcher" metadata::trusted yes
