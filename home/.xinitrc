#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)


if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

[[ -f ~/.Xresources ]] && xrdb -I$HOME ~/.Xresources
# xrdb ~/.Xresources
# xrdb ~/.homesick/repos/x-castle/home/.xresources/.Xresources.connermcd

# xmodmap ~/.Xmodmap
# xmodmap ~/.Xmodmap

unclutter -grab &

# xcompmgr -cCfF -r7 -o 0.25 -l-10 -t-8 -D7 &
xcompmgr -r7 -o 0.25 -l-10 -t-8 -D7 &
#devilspie -a &

# canto-fetch -db

# dropboxd

#eval $(ssh-agent)

# sh ~/.bin/.fehbg &

/usr/bin/VBoxClient-all

# exec gnome-session
# exec ck-launch-session dbus-launch --sh-syntax --exit-with-session startkde
# exec startxfce4
# exec awesome
# exec xterm -e emacs &
##exec xterm -e ranger &
##exec xterm &
# exec xterm -g 80x50 &
# ...or the Window Manager of your choice
# exec urxvt -g 174x31
# exec urxvt -g 136x26
# exec dbus-launch --exit-with-session emacs-27-vcs
exec ck-launch-session dbus-launch --sh-syntax --exit-with-session xmonad
