active_window=$(xdotool getactivewindow)

active_window_size=$(xdotool getwindowgeometry $active_window)
active_window_size=$(echo "$active_window_size" | sed -z 's/Window.*Geometry: //' | sed 's/x/ /')

active_monitor_size=$(xdotool getdisplaygeometry)

if [[ "$active_window_size" == "$active_monitor_size" ]]; then
    xdotool windowstate --remove FULLSCREEN $active_window
    xdotool windowstate --remove ABOVE $active_window
else
    xdotool windowstate --add ABOVE $active_window
    xdotool windowstate --add FULLSCREEN $active_window
fi
