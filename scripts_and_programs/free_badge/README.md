# Free Badge Bot

A discord bot with a single slash command that returns a string of text.

## Usage

- Add your discord bot token to `.env` after `TOKEN=`

    (DO NOT share your token with anyone, it is more valuable than a password)

- Run `app.py` (I have also included my deploy scripts which could help you deploy this on your own server)

## Commands

### General

- `/free_badge`: Returns the contents of a random line in `command_output.txt`.

## Dependencies

Python 3: [Download link](https://www.python.org/downloads/)

Python `nextcord` and `python-dotenv` modules: To install it, enter the following command in cmd or a terminal:

```
pip install nextcord
pip install python-dotenv
```

## Notes

The contents of `command_output.txt` are fetched on command use. You do not need to restart the bot to edit the command output.
