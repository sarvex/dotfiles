 

function fish_greeting
  fortune -a
end

function lolcat_handler --on-event fish_postexec
  seq 1 (tput cols) | sort -R | spark | lolcat
end
