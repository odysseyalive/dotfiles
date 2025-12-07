#!/bin/bash

# Volume plugin for Sketchybar

VOLUME=$(osascript -e 'output volume of (get volume settings)')
MUTED=$(osascript -e 'output muted of (get volume settings)')

if [ "$MUTED" = "true" ]; then
  ICON="婢"
elif [ "$VOLUME" -gt 60 ]; then
  ICON=""
elif [ "$VOLUME" -gt 30 ]; then
  ICON=""
elif [ "$VOLUME" -gt 0 ]; then
  ICON=""
else
  ICON=""
fi

sketchybar --set $NAME icon="$ICON" label="${VOLUME}%"
