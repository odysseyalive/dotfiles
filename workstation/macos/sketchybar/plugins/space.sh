#!/bin/bash

# AeroSpace workspace indicator plugin for Sketchybar

if command -v aerospace &> /dev/null; then
  SPACE=$(aerospace list-workspaces --focused 2>/dev/null)
  if [ -n "$SPACE" ]; then
    sketchybar --set $NAME label="[$SPACE]"
  else
    sketchybar --set $NAME label="[1]"
  fi
else
  sketchybar --set $NAME label=""
fi
