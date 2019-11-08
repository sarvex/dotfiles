#!/usr/bin/env bash
#  ____ _____ 
# |  _ \_   _|  Derek Taylor (DistroTube)
# | | | || |    http://www.youtube.com/c/DistroTube
# | |_| || |    http://www.gitlab.com/dwt1/ 
# |____/ |_| 
#
# This is a test script

myDir='/home/dt/test/'

dirOptions=$(cd ${myDir} && /bin/ls -d */ | cut -d " " -f 1)
dirChoice=$(echo -e "${dirOptions[@]}\nquit" | dmenu -i -p 'My directories: ')

if [ "$dirChoice" == "quit" ]; then
  echo "Program terminated."
elif [ "$dirChoice" == ${dirChoice} ]; then
  fileOptions=$(cd ${myDir}${dirChoice} && /bin/ls -Ap | cut -d " " -f 1 | grep -v / )
  fileChoice=$(echo -e "${fileOptions[@]}" | dmenu -i -l 15 -p 'Edit this file: ')
  exec emacsclient -c ${myDir}${dirChoice}${fileChoice}
fi
