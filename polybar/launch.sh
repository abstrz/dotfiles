#!/usr/bin/env sh

killall -q polybar

#wait until polybar is shut down.
while pgrep -u $UID -x polybar > /dev/null; do sleep .05; done

polybar -c ~/.config/polybar/main_top.ini main_top &
polybar -c ~/.config/polybar/main_bottom.ini main_bottom &
polybar -c ~/.config/polybar/side_bar1.ini side_bar1 &
polybar -c ~/.config/polybar/side_bar2.ini side_bar2 &
