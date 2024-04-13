# Windows URL File Opener

Opens windows `.url` files in the default browser on Linux.

# Usage

- Add `url_file_opener.py` to path.

- Set the default program for opening windows `.url` files to `url_file_opener.py`.

## Dependencies

Python 3: [Download link](https://www.python.org/downloads/)

# Notes

- This uses `xdg-open` to open links in the default browser. Python's built in `webbrowser.open` often fails to find the default browser and opens in a "random" browser instead.

- Wine can open `.url` files but it tends to be slow. This script has much lower overhead.
