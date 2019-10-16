#!/bin/bash
#  ____ _____ 
# |  _ \_   _|  Derek Taylor (DistroTube)
# | | | || |    http://www.youtube.com/c/DistroTube
# | |_| || |    http://www.gitlab.com/dwt1/ 
# |____/ |_| 
#
# Dmenu script for editing some of my more frequently edited config files.


declare options=(" awesome 
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
 vim 
 xmobar 
 xmonad 
 zsh 
 quit ")

choice=$(echo -e "${options[@]}" | dmenu -i -p 'Edit a config file: ')

	if [ "$choice" == ' quit ' ]; then
		echo "Program terminated."
	fi
	if [ "$choice" == ' awesome ' ]; then
        exec emacsclient -c '~/.config/awesome/rc.lua'
	fi
	if [ "$choice" == ' bash ' ]; then
        exec emacsclient -c '~/.bashrc'
	fi
	if [ "$choice" == ' bspwm ' ]; then
        exec emacsclient -c '~/.config/bspwm/bspwmrc'
	fi
	if [ "$choice" == ' dwm ' ]; then
        exec emacsclient -c '~/dwm/config.h'
	fi
	if [ "$choice" == ' emacs ' ]; then
        exec emacsclient -c '~/.emacs.d/init.el'
	fi
	if [ "$choice" == ' herbstluftwm ' ]; then
        exec emacsclient -c '~/.config/herbstluftwm/autostart'
	fi
	if [ "$choice" == ' i3 ' ]; then
        exec emacsclient -c '~/.i3/config'
	fi
	if [ "$choice" == ' polybar ' ]; then
        exec emacsclient -c '~/.config/polybar/config'
	fi
	if [ "$choice" == ' qtile ' ]; then
        exec emacsclient -c '~/.config/qtile/config.py'
	fi
	if [ "$choice" == ' st ' ]; then
        exec emacsclient -c '~/st/config.h'
	fi
	if [ "$choice" == ' sxhkd ' ]; then
        exec emacsclient -c '~/.config/sxhkd/sxhkdrc'
	fi
	if [ "$choice" == ' vim ' ]; then
        exec emacsclient -c '~/.vimrc'
	fi
	if [ "$choice" == ' xmobar ' ]; then
        exec emacsclient -c '~/.config/xmobar/xmobarrc2'
	fi
	if [ "$choice" == ' xmonad ' ]; then
        exec emacsclient -c '~/.xmonad/xmonad.hs'
	fi
	if [ "$choice" == ' zsh ' ]; then
        exec emacsclient -c '~/.zshrc'
	fi
