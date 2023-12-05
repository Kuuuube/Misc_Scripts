# Pixiv Auth Grabber

Grabs pixiv Access token and Refresh token.

# Usage

- Run `pixiv_auth.py`

- Log in to pixiv in the browser window

- Your Access token and Refresh token should print to the terminal window

# Dependencies

Python 3: [Download link](https://www.python.org/downloads/)

Python `selenium` module: To install it, enter the following command in cmd or a terminal:

```
pip install selenium
```

Chromedriver: [Download link](https://googlechromelabs.github.io/chrome-for-testing/) 

Find the list of `Stable` builds and open the link for the `chromedriver` version that supports your platform to download it. (Make sure to download `chromedriver` NOT `chrome`)

For normal windows installs: `win64`. For m1 mac: `mac-arm64`. For older mac: `mac-x64`. For linux: `linux64`.

Extract the zip and place the `chromedriver` binary file next to `pixiv_auth.py`. (Windows users will need to rename `chromedriver.exe` to `chromedriver`)

# Notes

- Refreshing an old Refresh token is also supported. Run `pixiv_auth.py --help` for full options.
