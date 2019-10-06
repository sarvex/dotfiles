#!/bin/bash
#  ____ _____ 
# |  _ \_   _|  Derek Taylor (DistroTube)
# | | | || |    http://www.youtube.com/c/DistroTube
# | |_| || |    http://www.gitlab.com/dwt1/ 
# |____/ |_| 
#
# Dmenu script for editing some of my more frequently edited config files.


declare -nc options=(" awesome 
 bash 
 bspwm 
 dwm 
 emacs 
 herbstluftwm 
 i3 
 polybar 
 qtile 
 st 
 sxhkd 
 xmonad 
 zsh 
 quit ")

choice=$(echo -e "${options[@]}" | dmenu -i -p 'Edit a config file: ')

	if [ "$choice" == ' quit ' ]; then
		echo "Program terminated."
	fi
	if [ "$choice" == ' awesome ' ]; then
        exec emacsclient -nc '~/.config/awesome/rc.lua'
	fi
	if [ "$choice" == ' bash ' ]; then
        exec emacsclient -nc '~/.bashrc'
	fi
	if [ "$choice" == ' bspwm ' ]; then
        exec emacsclient -nc '~/.config/bspwm/bspwmrc'
	fi
	if [ "$choice" == ' dwm ' ]; then
        exec emacsclient -nc '~/dwm/config.h'
	fi
	if [ "$choice" == ' emacs ' ]; then
        exec emacsclient -nc '~/.emacs.d/init.el'
	fi
	if [ "$choice" == ' herbstluftwm ' ]; then
        exec emacsclient -nc '~/.config/herbstluftwm/autostart'
	fi
	if [ "$choice" == ' i3 ' ]; then
        exec emacsclient -nc '~/.i3/config'
	fi
	if [ "$choice" == ' polybar ' ]; then
        exec emacsclient -nc '~/.config/polybar/config'
	fi
	if [ "$choice" == ' qtile ' ]; then
        exec emacsclient -nc '~/.config/qtile/config.py'
	fi
	if [ "$choice" == ' st ' ]; then
        exec emacsclient -nc '~/st/config.h'
	fi
	if [ "$choice" == ' sxhkd ' ]; then
        exec emacsclient -nc '~/.config/sxhkd/sxhkdrc'
	fi
	if [ "$choice" == ' xmonad ' ]; then
        exec emacsclient -nc '~/.xmonad/xmonad.hs'
	fi
	if [ "$choice" == ' zsh ' ]; then
        exec emacsclient -nc '~/.zshrc'
	fi
