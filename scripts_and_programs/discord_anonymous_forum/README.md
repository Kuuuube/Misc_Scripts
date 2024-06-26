# Discord Anonymous Forum

A discord bot that anonymizes messages and threads created in a forums channel.

## Usage

- Add your discord bot token to `.env` after `TOKEN=` 

    (DO NOT share your token with anyone, it is more valuable than a password)

- In `settings.json` under `enabled_guild_ids` add the guild IDs (server IDs) the bot will be active in.

- In `settings.json` under `forum_channel_ids` add the forum channel IDs the bot will be active in.

- Run `discord_anonymous_forum.py`

- Create threads in the active forum channels. Post in the threads using the `/p` command.

## Settings

- `enabled_guild_ids` `array`: List of guilds IDs (server IDs) the bot will be active in.

- `forum_channel_ids` `array`: List of forum channel IDs the bot will be active in.

- `blacklisted_roles` `array`: List of role IDs or names to disallow use of `/p`.

- `post_slowmode` `int`: Time in seconds required between the same user sending two `/p` commands.

- `restrict_duplicate_messages` `bool`: Whether or not to restrict users from sending the same message twice with `/p`.

- `per_day_ids` `bool`: Whether or not all message IDs should change daily.

- `per_thread_ids` `bool`: Whether or not message IDs for the same user should be different in each thread.

- `logging_enabled` `bool`: Whether or not to log which discord user IDs and their corresponding message IDs.

- `bot_dm_on_normal_message` `str`: The message sent by the bot when a user tries to send a message without using `/p`.

- `bot_embed_title_prefix` `str`: The part of the embed title after the post number and before the message ID.

- `bot_embed_title_suffix` `str`: The part of the embed title after the message ID.

- `attachment_prefix` `str`: The part of the embed body after the message and before the attachment link.

- `interaction_confirmation_prefix` `str`: The part of the default `/p` command response before the latency number.

- `interaction_confirmation_suffix` `str`: The part of the default `/p` command response after the latency number.

- `blacklisted_message` `str`: The message sent to users who have a role in the `blacklisted_roles` list upon trying to use `/p`.

- `wrong_channel_message` `str`: The message sent to users who try to use `/p` in a channel not in the `forum_channel_ids` list.

- `post_slowmode_error_message_prefix` `str`: The part of the message sent to users who try to post too fast using `/p` before the `post_slowmode` number.

- `post_slowmode_error_message_suffix` `str`: The part of the message sent to users who try to post too fast using `/p` after the `post_slowmode` number.

- `restrict_duplicate_messages_error_message` `str`: The messege sent to users who try to send a duplicate message using `/p`.

## Commands

### General

- `/p` `message` `(optional) attachment`: Used to post in anonymous threads.

### Moderator

- `/rename_thread` `new_thread_name`: Renames the thread this command is sent in. (Requires `Manage Threads` permission)

### Administrator

- `/check_id` `message_id`: Returns the discord user ID associated with the message ID if it has been logged.

- `/reload_settings`: Reloads the bot's settings from `settings.json`.

## Dependencies

Python 3: [Download link](https://www.python.org/downloads/)

Python `nextcord` module: To install it, enter the following command in cmd or a terminal:

```
pip install nextcord
```

## Notes

- Threads created in the forum channels are auto deleted and recreated by the bot.

- Messages can only be sent in forum threads using the slash command `/p`. Messages sent without `/p` are deleted. When typing a message that starts with `/`, discord does not show the user who is typing.

- Message IDs are salted and hashed from discord user IDs. They contain the first 9 characters of the resulting sha256 hash. Every time the bot is restarted or after one day, the salt is reset and all message IDs will change. This prevents potential reversing of the message IDs through reverse engineering of the code. There is also no possibility of leaking a secret key.

     It is okay to cut off the hash ONLY because hash collisions are both extremely unlikely and not a major issue in this case. NEVER do this for password hashing or any kind of security where collisions are a big deal.

- Due to bugs in the discord desktop client, on desktop, attachments can only be sent in slash commands while a forum thread is opened in full view.
