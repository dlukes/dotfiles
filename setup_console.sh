#!/usr/bin/env zsh

if [[ $(whoami) != "root" ]]; then
  >&2 echo "Must be root to run this script!"
  exit 1
fi

# get terminus font
pacman -S terminus-font
# get polished modern Czech qwerty keymap for console
curl https://raw.githubusercontent.com/lahwaacz/keymaps-czech-console/master/cz-qwerty.map >|/usr/share/kbd/keymaps/i386/qwerty/cz-qwerty.map

# NOTE: Currently, permanently setting these console preferences (to apply them
# at boot) is disabled, because it breaks Backspace, Delete, Ctrl+anything (and
# perhaps other things?).
# update vconsole settings
# cat <<END >/etc/vconsole.conf
# KEYMAP=cz-qwerty
# FONT=Lat2-Terminus16
# FONT_MAP=8859-2
# END

cat <<END
To load the keymap and font immediately, run:
  sudo loadkeys cz-qwerty
  setfont Lat2-Terminus16 -m 8859-2
Or:
  czech
... if you have already sourced .zshrc. To switch back to the us layout:
  english
END
