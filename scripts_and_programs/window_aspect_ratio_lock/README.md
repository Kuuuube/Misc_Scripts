# Window Aspect Ratio Lock

Script to lock the aspect ratio of a window when resizing.

## Usage

- Open the script in a text editor

- Change `windowName` to the window you want to lock the aspect ratio of.

    `SetTitleMatchMode` may also be changed for more control over window matching. See [SetTitleMatchMode](https://www.autohotkey.com/docs/v1/lib/SetTitleMatchMode.htm). 
    
    The default (`SetTitleMatchMode 2`) matches any window where `windowName` exists anywhere in the window title.

- Change `ratio` to the aspect ratio you want to use.

    For example, `(16 / 9)` is 16:9 aspect ratio but `1.77777777778` would also be accepted as 16:9 aspect ratio. 
    
    Do not enter a ratio formatted as `16:9`, it must evaluate to or equal the ratio's `(width / height)`.

- Optionally, if the window titlebar or similar is messing with your ratio, set an offset.

- Optionally, `sleepTime` may also be changed to edit how long to wait before checking if the window resize has stopped. 

    By default, if the window has received no further resizing within 250ms, the aspect ratio will be applied.

- Run the script using AutoHotkey

## Dependencies

AutoHotkey: [Download link](https://www.autohotkey.com/)

## Notes

- This script is not single instance. If you are messing around with the settings and closing and opening it multiple times, make sure to close it each time. The script will usually be found in the system tray.