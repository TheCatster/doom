#!/bin/sh
# Set keybinds for optimal Emacsing
. "$HOME/.doom.d/exwm/set_xmodmap.sh"

# Set default cursor, instead of the typical "x."
xsetroot -cursor_name left_ptr

# Start NetworkManager applet for easier WiFi access and preventing network-manager-config from crashing (why does that happen?)
nm-applet &

# Instead of launching multiple Emacsen, I already have a server
# running. Use that instead as the EDITOR.
export VISUAL=emacsclient
export EDITOR="$VISUAL"

# Allows scrolling in GTK 3 apps
export GDK_CORE_DEVICE_EVENTS=1

exec dbus-launch emacs --eval "(progn (add-to-list 'load-path \"~/.doom.d/exwm/\") (require 'catsters-exwm))" --eval "(require 'exwm)" -f exwm-enable
