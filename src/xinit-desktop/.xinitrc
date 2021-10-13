dwmstatus &
~/Scripts/screens.sh &
picom -b &
xset r rate 150 50 &
xinput --set-prop 10 'libinput Accel Profile Enabled' 0, 1 &
xsetroot -cursor_name left_ptr &
nitrogen --restore &
bluetoothctl power on &
steam-tweaks &
xinput disable \"Sony Computer Entertainment Wireless Controller Touchpad\" &
doas mount /dev/sdc ~/Drives/usb &
nitrogen --restore &
exec dwm 
