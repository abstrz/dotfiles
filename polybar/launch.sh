#!/usr/bin/env sh

killall -q polybar

#wait until polybar is shut down.
while pgrep -u $UID -x polybar > /dev/null; do sleep .1; done

polybar -c ~/.config/polybar/config main &
