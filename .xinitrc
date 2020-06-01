#!/bin/sh
userresources=$HOME/.Xresources
# merge in defaults and keymaps
if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

xrandr --output DisplayPort-1 --off --output DisplayPort-2 --off --output HDMI-A-3 --mode 1920x1080 --pos 3280x0 --rotate normal --output HDMI-A-4 --mode 1360x768 --pos 0x0 --rotate normal --output DVI-D-0 --primary --mode 1920x1080 --pos 1360x0 --rotate normal --output HDMI-1-1 --off --output HDMI-1-2 --off --output DP-1-1 --off --output HDMI-1-3 --off  &
picom -b &
xset r rate 150 35 &
feh --bg-max ~/Pictures/Wallpapers/7jyjaq73v2251.jpg --bg-fill ~/Pictures/Wallpapers/wp5317145.jpg --bg-fill ~/Pictures/Wallpapers/cyberpunk-neon-science-fiction-police-sg.jpg &
ntpd -qg &
dropbox &
dunst &
ranger &
dwmstatus 2>&1 >/dev/null &
exec dbus-launch --exit-with-session dwm

