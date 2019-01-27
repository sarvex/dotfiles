#! /bin/bash 
compton --config ~/.config/compton/compton.conf &
nitrogen --restore &
urxvtd -q -o -f &

while true; do
    xsetroot -name "` date +"%b %e, %Y - %R" `"
    sleep 1m    # Update time every minute
done &
