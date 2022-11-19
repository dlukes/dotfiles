#!/bin/sh

set -euf

# Stuff you might want to pull in on every Fedora install to make your life easier.



# ------------------------------------------------------ Install/enable additional repos {{{1


sudo dnf in -by \
  https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm \
  https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
sudo dnf config-manager --set-enabled fedora-cisco-openh264 google-chrome



# ----------------------------------------------------------------- Software compilation {{{1


sudo dnf grp install -by "Development Tools" "Development Libraries"
sudo dnf in -by gcc-c++ libstdc++-devel libtool



# ------------------------------------------------------------ GNOME tweaks & extensions {{{1


sudo dnf in -by \
  papirus-icon-theme \
  gnome-tweaks \
  gnome-shell-extension-appindicator \
  gnome-shell-extension-caffeine \
  gnome-shell-extension-freon \
  gnome-shell-extension-gamemode \
  gnome-shell-extension-openweather \
  gnome-shell-extension-system-monitor-applet

# Suggestions:
#
# - gnome-shell-extension-pop-shell if you want a more sophisticated tiling window
#   environment (you probably don't, you're used to GNOME Shell as it is)
# - gnome-shell-extension-netspeed for a dedicated network speed indicator, but the
#   system monitor applet has this too and it looks less distracting

# TODO: Possibly auto-enable extensions with gnome-extensions enable? But since you need
# to install some of them manually anyway, maybe not worth the hassle.



# ----------------------------------------------------------------------- Media playback {{{1


# Just following the guidelines from the quick docs:
#
# - https://docs.fedoraproject.org/en-US/quick-docs/assembly_installing-plugins-for-playing-movies-and-music/
# - https://docs.fedoraproject.org/en-US/quick-docs/openh264/

sudo dnf in -by \
  gstreamer1-plugins-{bad-\*,good-\*,base} \
  gstreamer1-plugin-openh264 \
  gstreamer1-libav \
  --exclude=gstreamer1-plugins-bad-free-devel \
  lame\* \
  --exclude=lame-devel \
  mozilla-openh264
sudo dnf grp install -by --with-optional Multimedia



# --------------------------------------------------------------------- Various software {{{1


sudo dnf in -by \
  vim fish ffmpeg sox vlc \
  aspell aspell-en aspell-cs aspell-fr aspell-es \
  tilix tilix-nautilus \
  nextcloud-client nextcloud-client-nautilus



# ------------------------------------------------------------------------- Flatpak apps {{{1


flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
flatpak install flathub \
  org.zotero.Zotero \
  com.google.Chrome \
  com.skype.Client \
  org.gimp.GIMP \
  org.onlyoffice.desktopeditors \
  io.podman_desktop.PodmanDesktop \
  pm.mirko.Atoms



# --------------------------------------------------------------------------------- Bye! {{{1


>&2 echo 'Check out your Org-roam Fedora node for possible follow-up tasks.'
