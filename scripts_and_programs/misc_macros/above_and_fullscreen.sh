active_window=$(xdotool getactivewindow)
active_window_state=$(xprop -id "$active_window" | grep "_NET_WM_STATE(ATOM)" | grep -o "_NET_WM_STATE_FULLSCREEN")

if [[ "$active_window_state" == "_NET_WM_STATE_FULLSCREEN" ]]; then
    xdotool windowstate --remove FULLSCREEN $active_window
    xdotool windowstate --remove ABOVE $active_window
else
    xdotool windowstate --add ABOVE $active_window
    xdotool windowstate --add FULLSCREEN $active_window
fi
