#!/usr/bin/env bash


#BAR_ICON=""
NOTIFY_ICON=/usr/share/icons/Papirus/32x32/apps/system-software-update.svg

get_total_updates() {
    EBUILDS=$(doas emerge --pretend -uDU --keep-going --with-bdeps=y @world | grep ebuild | wc -l)
}

while true; do
    get_total_updates
    wait
    while (( EBUILDS > 0 )); do
        if (( EBUILDS == 1 )); then
            echo "$EBUILDS ebuild"
        elif (( EBUILDS > 1)); then
            echo "$EBUILDS ebuilds"
        else
            echo $BAR_ICON 
        fi
        sleep 1h
        get_total_updates
        wait
    done


    while (( EBUILDS == 0 )); do
        echo $BAR_ICON
        sleep 1h
        get_total_updates
        wait
    done
done
