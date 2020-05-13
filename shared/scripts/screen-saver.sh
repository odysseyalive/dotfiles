#!/bin/bash
# Dependency checks
if [ ! $(command -v xrandr) ]; then
    echo "\"command not found: \"xrandr\"" >&2
    exit 1
fi
if [ ! $(command -v urxvt) ]; then
    echo "\"command not found: \"urxvt\"" >&2
    exit 1
fi
if [ ! $(command -v pyxtrlock) ]; then
    echo "\"command not found: \"pyxtrlock\"" >&2
    exit 1
fi
if [ ! $(command -v asciiquarium) ]; then
    echo "\"command not found: \"asciiquarium\"" >&2
    exit 1
fi

# kill any current instances of screensaver
pkill -f asciiquarium

# starts screensaver on all monitors
OFFSETS=(`xrandr -q | grep " connected " | xargs -l | cut -d " " -f1`)
if [ ${#OFFSETS[@]} -gt 1 ]; then
    xrandr --output ${OFFSETS[0]} --output ${OFFSETS[1]} --same-as ${OFFSETS[0]}
else
    xrandr --output ${OFFSETS[0]} --primary
fi
sleep 2 
urxvt -e sh -c "wmctrl -x -r urxvt -b add,fullscreen; asciiquarium" &

# Store the current layout and set the default one
if [ $(command -v xkblayout-state) ]; then
    CURRENT=$(xkblayout-state print %c)
    xkblayout-state set 0
fi

# Lock screen, reset monitors and kill screen saver on unlock
pyxtrlock 
bash ~/.config/scripts/screen-init.sh 
pkill -f asciiquarium 

# Restore kb layout
if [ $(command -v xkblayout-state) ]; then
    xkblayout-state set $CURRENT
fi

