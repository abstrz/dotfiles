#!/bin/sh
userresources=$HOME/.Xresources
# merge in defaults and keymaps
if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi


#from siji font install
xset +fp ~/.local/share/fonts
xset fp rehash

desktop=bspwm

exec $desktop

