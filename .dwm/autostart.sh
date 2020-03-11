#! /bin/bash 
compton --config ~/.config/compton/compton.conf &
nitrogen --restore &
urxvtd -q -o -f &

dte(){
  dte="$(date +"%A, %B %d - %l:%M%p")"
  echo -e "🕒 $dte"
}

hdd() {
  hdd="$(df -h | awk 'NR==4{print $3, $5}')"
  echo -e "💿 $hdd"
}

mem(){
  mem=`free | awk '/Mem/ {printf "%d MiB/%d MiB\n", $3 / 1024.0, $2 / 1024.0 }'`
  echo -e "🖪 $mem"
}

cpu(){
  read cpu a b c previdle rest < /proc/stat
  prevtotal=$((a+b+c+previdle))
  sleep 0.5
  read cpu a b c idle rest < /proc/stat
  total=$((a+b+c+idle))
  cpu=$((100*( (total-prevtotal) - (idle-previdle) ) / (total-prevtotal) ))
  echo -e "💻 $cpu% cpu"
}

pulse () {
    volume=$(pactl list sinks | grep '^[[:space:]]Volume:' | head -n $(( $SINK + 1 )) | tail -n 1 | sed -e 's,.* \([0-9][0-9]*\)%.*,\1,')
    status=$(pacmd list-sinks | awk '/muted/ { print $2 }')

        if [ "$status" = "yes" ]; then
            echo -e "🔈 muted"
        else
            echo -e "🔈 $volume%"
        fi
}

while true; do
     xsetroot -name "$(cpu) | $(mem) | $(hdd) | $(pulse) | $(dte)"
     sleep 1s    # Update time every one second(s)
done &
