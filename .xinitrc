#!/bin/sh
userresources=$HOME/.Xresources
# merge in defaults and keymaps
if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi


#from siji font install
xset +fp ~/.local/share/fonts
xset fp rehash

#startx integration with elogind


xrandr --output DisplayPort-1 --off --output DisplayPort-2 --off --output HDMI-A-3 --mode 1920x1080 --pos 3280x0 --rotate normal --output HDMI-A-4 --mode 1360x768 --pos 0x0 --rotate normal --output DVI-D-0 --primary --mode 1920x1080 --pos 1360x0 --rotate normal --output HDMI-1-1 --off --output HDMI-1-2 --off --output DP-1-1 --off --output HDMI-1-3 --off  &
picom -b &
xset r rate 150 35 &
feh --bg-max ~/pictures/wallpapers/zp412tfvsyi41.jpg --bg-fill ~/pictures/wallpapers/59yy03w98li41.jpg --bg-fill ~/pictures/wallpapers/emss7dc0tgr41.png &
ntpd -qg &
dropbox &
dunst &
ranger &
dwmstatus 2>&1 >/dev/null &
exec dbus-launch --exit-with-session dwm

