# Misc Macros

Misc macros for things such as window management and keyboard shortcuts.

## Above and Fullscreen

Uses `xdotool` to set the active window to fullscreen and above. Workaround for xfwm4 and xfce panel not respecting fullscreen when the fullscreen window loses focus causing the panel to pop up over the fullscreen window.

### Notes

- xfwm4 does not allow editing `ABOVE` for fullscreen windows. The order of commands in the script is very important. `ABOVE` is always edited before activating fullscreen or after leaving fullscreen.
