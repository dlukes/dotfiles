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
  gnome-shell-extension-sound-output-device-chooser \
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
  io.podman_desktop.PodmanDesktop



# --------------------------------------------------------------------------------- Bye! {{{1


cat <<'EOF' >&2

========================================================================================
Is this a machine with an Nvidia GPU? In that case, check if you have secure boot
enabled, and if so, import your self-generated key which will be used to sign kernel
modules:

  https://rpmfusion.org/Howto/Secure%20Boot !!!!

Don't worry if kmodgenca says there already is a certificate, that's fine. The password
is entered only at the mokutil invocation.

Then, install the appropriate kernel module and additional CUDA package:

  https://rpmfusion.org/Howto/NVIDIA#Installing_the_drivers

To check whether the module is available:

  modinfo -F version nvidia

To force rebuilding it and possibly inspect any error output (I think):

  sudo akmods --force && sudo dracut --force # to rebuild mods?

You might run into issues with suspend. If so, check out the tips here:

  https://rpmfusion.org/Howto/NVIDIA#Suspend

(Although it looks like these are already installed and enabled by default when
installing the kernel module above.)

Some more tips and tricks:

  https://wiki.archlinux.org/title/NVIDIA/Tips_and_tricks

Among other things, this page suggests disabling nvidia-resume.service, because
/usr/lib/systemd/system-sleep/nvidia does the same but earlier, and it's enabled by
default. In practice though, my experience has been that if suspend works, it works
regardless of this service being enabled or not.

========================================================================================
Is this a machine with RGB lighting? In that case, you'll want to install OpenRGB. RPM
has the advantage of setting up the required udev rules for SMBus access, Flatpak has
the advantage of potentially being more up-to-date:

  dnf in -by openrgb
  flatpak install flathub org.openrgb.OpenRGB

I2C kernel modules also need to be enabled, i2c-dev and i2c-piix4 (the latter for AMD,
there are others for Intel). Do this manually with modprobe, or put them into
/etc/modules-load.d/smbus.conf to load them automatically.

On first run, don't forget to resize (A)RGB header zones (you'll get a prompt) and
configure an "off" profile. You can then copy OpenRGB's desktop file into
~/.config/autostart and append "--profile off --startminimized" to the Exec line.
EOF
