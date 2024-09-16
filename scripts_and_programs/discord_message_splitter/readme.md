# Discord Message Splitter

Splits long discord messages into parts to be posted individually.

## Usage

1. Set `max_message_chars` to the number of chars you want the message to be split at.

    Currently, the limit is `2000` for normal users and `4000` for nitro users.

2. Run `discord_message_splitter.py` and enter the file containing the message to split.

3. The first message will now be copied to your clipboard. Paste it wherever you need, then hit enter to copy the next message.

## Dependencies

Python 3: [Download link](https://www.python.org/downloads/)

## Notes

- Does not support messages with a single line longer than `max_message_chars` (2000 by default).
