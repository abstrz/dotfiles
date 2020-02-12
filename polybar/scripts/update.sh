#!/usr/bin/env bash


#BAR_ICON=""
NOTIFY_ICON=/usr/share/icons/Papirus/32x32/apps/system-software-update.svg

get_total_updates() {
    UPDATES=$(checkupdates 2>/dev/null | wc -l);
}

while true; do
    get_total_updates
    if hash notify-send &>/dev/null; then
        if (( UPDATES > 50 )); then
            notify-send -u critical -i $NOTIFY_ICON "THERE ARE $UPDATES UPDATES AWAITING YOU, SIRE."
        elif (( UPDATES > 25 )); then
            notify-send -u normal -i $NOTIFY_ICON "THERE ARE $UPDATES UPDATES AWAITING YOU, SIRE."
        elif (( UPDATES > 2 )); then
            notify-send -u low -i $NOTIFY_ICON "THERE ARE $UPDATES UPDATES AWAITING YOU."
        fi
    fi

    while (( UPDATES > 0 )); do
        if (( UPDATES == 1 )); then
            echo "$UPDATES Update"
        elif (( UPDATES > 1)); then
            echo "$UPDATES Updates"
        else
            echo $BAR_ICON 
        fi
        sleep 10
        get_total_updates
    done


    while (( UPDATES == 0 )); do
        echo $BAR_ICON
        sleep 1800
        get_total_updates
    done
done
