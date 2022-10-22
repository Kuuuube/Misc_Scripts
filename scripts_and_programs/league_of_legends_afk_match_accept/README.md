# League of Legends AFK Match Accept

Automatically accepts League of Legends match ready checks by moving the mouse into position and clicking where the accept button is.

## Usage

- Set the mouse position X and Y offset for the client window in `config.ini`. Ideally you want this to also be inbetween two champions when in champ select to avoid unintentional hovering/banning of champions while you are afk before you turn off the script.

    (If you mess something up, delete `config.ini` and it will re-generate with default settings)

- Press `Alt + w` to start the script. Press `Alt + q` to pause the script. Press `Ctrl + Alt + w` to exit the script. (It can also be exited by right clicking and closing it in the system tray)

## Dependencies

Install [AutoHotKey](https://www.autohotkey.com/) if you want to run from the .ahk file. This step is not required when using the .exe file.

## Notes

- As far as I know, doing this has never gotten anyone banned.

- Don't forget the hotkeys to pause and exit the script or it may be difficult to exit the script due to it controlling your mouse.

- If the League of Legends client ever changes its window name you can edit line 42 of `league_afk_match_accept.ahk` to the new value to make the script work again.

    For example: Changing `if WinExist("League of Legends")` to `if WinExist("Whatever riot changed it to")` will search for a window named `Whatever riot changed it to` instead of `League of Legends`.