#!/bin/sh
userresources=$HOME/.Xresources
# merge in defaults and keymaps
if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"

fi

desktop=bspwm

exec $desktop

