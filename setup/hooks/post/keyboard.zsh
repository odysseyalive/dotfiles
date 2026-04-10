#!/usr/bin/env zsh

echo "# # Setting up Caps Lock / Escape swap for macOS..."
if [[ "$(uname)" == "Darwin" ]]; then
  mkdir -p ~/Library/LaunchAgents
  cat >~/Library/LaunchAgents/com.local.KeyRemapping.plist <<'INNER_EOF'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>com.local.KeyRemapping</string>
    <key>ProgramArguments</key>
    <array>
        <string>/usr/bin/hidutil</string>
        <string>property</string>
        <string>--set</string>
        <string>{"UserKeyMapping":[{"HIDKeyboardModifierMappingSrc":0x700000039,"HIDKeyboardModifierMappingDst":0x700000029},{"HIDKeyboardModifierMappingSrc":0x700000029,"HIDKeyboardModifierMappingDst":0x700000039}]}</string>
    </array>
    <key>RunAtLoad</key>
    <true/>
</dict>
</plist>
INNER_EOF

  hidutil property --set '{"UserKeyMapping":[{"HIDKeyboardModifierMappingSrc":0x700000039,"HIDKeyboardModifierMappingDst":0x700000029},{"HIDKeyboardModifierMappingSrc":0x700000029,"HIDKeyboardModifierMappingDst":0x700000039}]}'

  echo "  Caps Lock and Escape keys swapped (persists across reboots)"
else
  echo "Keyboard remapping is currently only implemented for macOS."
fi
