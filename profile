site_profile=/usr/share/defaults/etc/profile
if [ -s "$site_profile" ]; then
  . "$site_profile"
fi

us=en_US.utf-8
gb=en_GB.utf-8

export LANG=$us

export LC_CTYPE=$us
export LC_NUMERIC=$us
export LC_COLLATE=$us
export LC_MONETARY=$us
export LC_MESSAGES=$us
export LC_NAME=$us
export LC_ADDRESS=$us
export LC_TELEPHONE=$us
export LC_IDENTIFICATION=$us

export LC_TIME=$gb
export LC_PAPER=$gb
export LC_MEASUREMENT=$gb

# vi: set ft=sh:
