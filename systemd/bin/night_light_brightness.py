#!/usr/bin/python3

# See https://gitlab.freedesktop.org/geoclue/geoclue/ for details on Geoclue. There's
# both a D-Bus API and a regular C one accessible via PyGObject. For an example of how
# to use the D-Bus one, the first version of this script is attached to your D-Bus
# Org-Roam node. However, once you figure out how the translation works
# (gclue_simple_new_sync -> Geoclue.Simple.new_sync), the C bindings are *much* simpler
# to use, so I'm going with those.

# TODO: time.sleep is the wrong API to use here, as the counter only increments when the
# computer itself is running. So if the computer sleeps, the sleep is paused, and then
# resumed where it left off then the computer wakes up. If you invoke time.sleep(60) and
# then immediately put the computer to sleep, then wake it 8 hourse later, Python will
# wait for another 60s and then wake up. That's not what you want here.
#
# Instead, you should probably use `systemd-run --on-calendar=...` to schedule a one-off
# timer, either calling ddcutil directly, or this script (distinguish via sys.argv[1],
# which could be either e.g. schedule or apply?).
#
# However, there are additional complications. What happens when the computer is asleep
# when the timer triggers? Should it be Persistent? What happens when multiple
# Persistent timers accumulate over a long period of sleep?
#
# Probably not worth spending the time and energy to get all of this right at this
# point, because since I've started using the GNOME extension what allows me to set
# monitor brightness via a keyboard shortcut or the top panel[1], changing it is much
# less of a pain. So better just completely disable this for now.
#
# [1]: https://extensions.gnome.org/extension/2645/brightness-control-using-ddcutil/
#
# Or maybe even simpler: don't use systemd timers at all, just have a long-running
# service that wakes up every few minutes to check whether it should toggle itself from
# day mode to night mode or back? That would make it straightforward to use a
# level-triggered approach, since you can keep state around, as opposed to an
# edge-triggered one, which is always trickier to get right.

import sys

sys.exit()

from datetime import datetime
import subprocess as sp
import sys
import time

from astral import LocationInfo, sun
import gi
import pytz

gi.require_version("Geoclue", "2.0")
from gi.repository import Geoclue

try:
    from config.night_light_brightness import sunrise_cmds, sunset_cmds
except ImportError:
    print("No config found, using defaults.", file=sys.stderr)
    sunrise_cmds = [["ddcutil", "setvcp", "x10", "10"]]
    sunset_cmds = [["ddcutil", "setvcp", "x10", "0"]]

# The first argument should be the basename of the desktop file of the app requesting
# the location, but it can just be a dummy value.
gclue = Geoclue.Simple.new_sync("dummy", Geoclue.AccuracyLevel.NEIGHBORHOOD, None)
gloc = gclue.get_location()
lat, lon = gloc.get_property("latitude"), gloc.get_property("longitude")

# The first two arguments are name and region, and can be dummies for custom locations.
# The time zone name should be chosen from among pytz.all_timezones, which doesn't
# contain CEST, so you probably don't have to worry about daylight savings time
# adjustments, astral takes care of them.
aloc = LocationInfo("dummy", "dummy", time.tzname[0], lat, lon)
sunrise = sun.sunrise(aloc.observer)
sunset = sun.sunset(aloc.observer)
now = datetime.now(pytz.UTC)
time_to_sunrise = sunrise - now
time_to_sunset = sunset - now
day = 3600 * 24

if 0 < time_to_sunrise.total_seconds() < day:
    sleep = time_to_sunrise
    cmds = sunrise_cmds
elif 0 < time_to_sunset.total_seconds() < day:
    sleep = time_to_sunset
    cmds = sunset_cmds
else:
    print("No upcoming sunrise or sunset today, exiting.", file=sys.stderr)
    sys.exit()

print(f"Sleeping for {sleep}, then running: {cmds}", file=sys.stderr)
time.sleep(sleep.total_seconds())
for cmd in cmds:
    print(f"Running: {cmd}", file=sys.stderr)
    sp.run(cmd, check=True)
print("Done. Let monitor brightness be merciful upon thine eyes!", file=sys.stderr)
