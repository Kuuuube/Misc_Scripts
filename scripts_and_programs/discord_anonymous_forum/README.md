# Discord Anonymous Forum

A discord bot that anonymizes messages and threads created in a forums channel.

## Usage

- Add your discord bot token to `.env` after `TOKEN=` 

    (DO NOT share your token with anyone, it is more valuable than a password)

- In `discord_anonymous_forum.py` under `# General configuration` add the `enabled_guild_ids` (server IDs) and `forum_channel_ids` the bot will be active in.

- Run `discord_anonymous_forum.py`

## Dependencies

Python 3: [Download link](https://www.python.org/downloads/)

Python `nextcord` module: To install it, enter the following command in cmd or a terminal:

```
pip install nextcord
```

## Notes

- Threads in the forum channel are auto deleted and recreated by the bot.

- Messages can only be sent in forums using the slash commands `/p`. When typing a message that starts with `/` discord does not show the user who is typing.

- If a user tries to send a message without a slash command the message is deleted and they are sent a DM explaining that they cannot send messages like that.

- Message IDs are salted and hashed from discord user IDs. They contain the first 9 characters of the resulting sha256 hash. Every time the bot is restarted or after one day, the salt is reset and all message IDs will change. This prevents potential reversing of the message IDs through reverse engineering of the code. There is also no possibility of leaking a secret key.

     It is okay to cut off the hash ONLY because hash collisions are both extremely unlikely and not a major issue in this case. NEVER do this for password hashing or any kind of security where collisions are a big deal.

- Discord user IDs and their corresponding message IDs are logged to `userid_log.json`. Users with the `Administrator` permission can use `/check_id` to find the discord user ID attached to a message ID. To turn off logging, under `# General configuration` change `logging_enabled` to `False`.

- By default, a user can only send a message using `/p` every 5 seconds and cannot send multiple duplicate messages in a row.

- Due to bugs in the discord desktop client, on desktop, attachments can only be sent in threads using slash commands while a forum thread is opened in full view.
