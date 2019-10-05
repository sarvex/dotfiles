#!/bin/bash
#  ____ _____ 
# |  _ \_   _|  Derek Taylor (DistroTube)
# | | | || |    http://www.youtube.com/c/DistroTube
# | |_| || |    http://www.gitlab.com/dwt1/ 
# |____/ |_| 
#
# Dmenu script for editing some of my more frequently edited config files.


declare -a options=(" awesome 
 bash 
 bspwm 
 dwm 
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
        exec emacs $HOME/.config/awesome/rc.lua
	fi
	if [ "$choice" == ' bash ' ]; then
        exec emacs $HOME/.bashrc
	fi
	if [ "$choice" == ' bspwm ' ]; then
        exec emacs $HOME/.config/bspwm/bspwmrc
	fi
	if [ "$choice" == ' dwm ' ]; then
        exec emacs $HOME/dwm/config.h
	fi
	if [ "$choice" == ' herbstluftwm ' ]; then
        exec emacs $HOME/.config/herbstluftwm/autostart
	fi
	if [ "$choice" == ' i3 ' ]; then
        exec emacs $HOME/.i3/config
	fi
	if [ "$choice" == ' polybar ' ]; then
        exec emacs $HOME/.config/polybar/config
	fi
	if [ "$choice" == ' qtile ' ]; then
        exec emacs $HOME/.config/qtile/config.py
	fi
	if [ "$choice" == ' st ' ]; then
        exec emacs $HOME/st/config.h
	fi
	if [ "$choice" == ' sxhkd ' ]; then
        exec emacs $HOME/.config/sxhkd/sxhkdrc
	fi
	if [ "$choice" == ' xmonad ' ]; then
        exec emacs $HOME/.xmonad/xmonad.hs
	fi
	if [ "$choice" == ' zsh ' ]; then
        exec emacs $HOME/.zshrc
	fi
