#!/bin/bash
#  ____ _____ 
# |  _ \_   _|  Derek Taylor (DistroTube)
# | | | || |    http://www.youtube.com/c/DistroTube
# | |_| || |    http://www.gitlab.com/dwt1/ 
# |____/ |_| 
#
# Dmenu script for reddio - a command line Reddit viewer.


declare -a options=(" r/archlinux 
 r/commandline 
 r/DistroTube 
 r/linux 
 r/linuxmasterrace 
 r/unixporn 
 r/linux4noobs 
 r/vim 
 quit ")

choice=$(echo -e "${options[@]}" | dmenu -l -i -p 'Last 10 Posts From Reddit: ')

	if [ "$choice" == ' quit ' ]; then
		echo "Program terminated."
	fi
	if [ "$choice" == ' r/archlinux ' ]; then
        exec st -e $SHELL -c 'reddio print -l 10 r/archlinux;$SHELL'
	fi
	if [ "$choice" == ' r/commandline ' ]; then
        exec st -e $SHELL -c 'reddio print -l 10 r/commandline;$SHELL'
	fi
	if [ "$choice" == ' r/DistroTube ' ]; then
        exec st -e $SHELL -c 'reddio print -l 10 r/DistroTube;$SHELL'
	fi
	if [ "$choice" == ' r/GopherHoles ' ]; then
        exec st -e $SHELL -c 'reddio print -l 10 r/GopherHoles;$SHELL'
	fi
	if [ "$choice" == ' r/linux ' ]; then
        exec st -e $SHELL -c 'reddio print -l 10 r/linux;$SHELL'
	fi
	if [ "$choice" == ' r/linuxmasterrace ' ]; then
        exec st -e $SHELL -c 'reddio print -l 10 r/linuxmasterrace;$SHELL'
	fi
	if [ "$choice" == ' r/unixporn ' ]; then
        exec st -e $SHELL -c 'reddio print -l 10 r/unixporn;$SHELL'
	fi
	if [ "$choice" == ' r/linux4noobs ' ]; then
        exec st -e $SHELL -c 'reddio print -l 10 r/linux4noobs;$SHELL'
	fi
	if [ "$choice" == ' r/vim ' ]; then
        exec st -e $SHELL -c 'reddio print -l 10 r/vim;$SHELL'
	fi
