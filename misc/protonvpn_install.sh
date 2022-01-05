#!/bin/sh

set -euf
script_dir=$(dirname "$(realpath "$0")")
. "$script_dir"/util.sh

if ! command -v eopkg >/dev/null; then
  >&2 echo -n "\
This script is only meant for Solus Linux. In particular, on macOS, use the GUI
installer from <https://protonvpn.com/download-macos>.
"
  exit 1
fi

# Deps: hopefully, the CLI installs some deps also needed by the GUI.
sudo eopkg install protonvpn-cli
pip install dbus-python

org=ProtonVPN
for pkg in proton-python-client protonvpn-nm-lib linux-app; do
  org=Proton$([ "$pkg" = proton-python-client ] && echo Mail || echo VPN)
  ver=$(github_latest_release_tag_name $org $pkg)
  pip install -r https://raw.githubusercontent.com/$org/$pkg/$ver/requirements.txt || true
  pip install git+https://github.com/$org/$pkg@$ver
done

# Patch support for Ayatana app indicators, see
# <https://github.com/ProtonVPN/linux-app/issues/39>
protonvpn_gui_dir=$(find "$HOME"/.local -type d -name protonvpn_gui)
(
  cd "$protonvpn_gui_dir"
  patch -p1 <<'EOF'
--- a/view/indicator.py	2021-08-31 15:47:14.420742423 +0200
+++ b/view/indicator.py	2021-08-31 15:47:33.515563589 +0200
@@ -103,8 +103,12 @@
     ERROR_PATH = os.path.join(ICON_DIR_PATH, VPN_TRAY_ERROR)

     def __init__(self, application):
-        gi.require_version("AppIndicator3", "0.1")
-        from gi.repository import AppIndicator3 as appindicator
+        try:
+            gi.require_version("AppIndicator3", "0.1")
+            from gi.repository import AppIndicator3 as appindicator
+        except ValueError:
+            gi.require_version("AyatanaAppIndicator3", "0.1")
+            from gi.repository import AyatanaAppIndicator3 as appindicator
         self.setup_reply_subject()
         self.__application = application
         self.__generate_menu()
EOF
)

launcher="$HOME/.local/share/applications/protonvpn.desktop"
cat <<EOF >"$launcher"
[Desktop Entry]
Name=ProtonVPN
Comment=GUI for ProtonVPN
Exec=$(pyenv which protonvpn)
Icon=protonvpn-gui
Type=Application
Terminal=false
Categories=Network;
Keywords = privacy;secret;censorship;geoblocking;
EOF
dbus-launch gio set "$launcher" metadata::trusted yes
sudo usysconf run

>&2 echo -n "\
========================================================================================

Debugging tips:

- logs are written to protonvpn-gui.log in the XDG cache dir
- set PROTONVPN_DEBUG=true for maximum logging detail
- set PROTONVPN_DEBUG_CONSOLE=true to log to terminal as well

If any of these don't seem to work, stuff has probably changed. Check the installed
source code in:

$protonvpn_gui_dir
"
