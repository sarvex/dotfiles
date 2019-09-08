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
        exec st -e './.dmenu/dmenu-reddio-archlinux.sh'
	fi
	if [ "$choice" == ' r/commandline ' ]; then
        exec st -e './.dmenu/dmenu-reddio-commandline.sh'
	fi
	if [ "$choice" == ' r/DistroTube ' ]; then
        exec st -e './.dmenu/dmenu-reddio-distrotube.sh'
	fi
	if [ "$choice" == ' r/linux ' ]; then
        exec st -e './.dmenu/dmenu-reddio-linux.sh'
	fi
	if [ "$choice" == ' r/linuxmasterrace ' ]; then
        exec st -e './.dmenu/dmenu-reddio-linuxmasterrace.sh'
	fi
	if [ "$choice" == ' r/unixporn ' ]; then
        exec st -e './.dmenu/dmenu-reddio-unixporn.sh'
	fi
	if [ "$choice" == ' r/linux4noobs ' ]; then
        exec st -e './.dmenu/dmenu-reddio-linux4noobs.sh'
	fi
	if [ "$choice" == ' r/vim ' ]; then
        exec st -e './.dmenu/dmenu-reddio-vim.sh'
	fi
