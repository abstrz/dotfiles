#!/usr/bin/env sh

## Add this to your wm startup file.

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

polybar -c ~/.config/polybar/config_top.ini main & 
polybar -c ~/.config/polybar/config_top.ini side1 & 
polybar -c ~/.config/polybar/config_top.ini side2 &
polybar -c ~/.config/polybar/config_bottom.ini main

