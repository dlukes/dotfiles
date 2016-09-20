#!/usr/bin/env zsh

if [[ $(whoami) != "root" ]]; then
  >&2 echo "Must be root to run this script!"
  exit 1
fi

# get terminus font
sudo pacman -S terminus-font
# get polished modern Czech qwerty keymap for console
# wget https://raw.githubusercontent.com/lahwaacz/keymaps-czech-console/master/cz-qwerty.map -o /usr/share/kbd/keymaps/i386/qwerty/cz-qwerty.map

# update vconsole settings
cat <<END >/etc/vconsole.conf
KEYMAP=cz-qwerty
FONT=Lat2-Terminus16
FONT_MAP=8859-2
END

cat <<END
To load the keymap and font immediately, run:
  sudo loadkeys cz-qwerty
  setfont Lat2-Terminus16 -m 8859-2
END
