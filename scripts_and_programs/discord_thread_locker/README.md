# Discord Thread Locker

A simple discord bot that locks and archives discord threads after a set amount of time.

## Usage

- Add your discord bot token to `.env` after `TOKEN=` 

    (DO NOT share your token with anyone, it is more valuable than a password)

- In `discord_thread_locker.py` under `# List of specific channel IDs to check` add a list of channels to check for threads in.

    Channel ids in `eng_channel_ids` will be sent bot messages in English. Channel ids in `jp_channel_ids` will be sent bot messages in Japanese.

- Run `discord_thread_locker.py`

## Dependencies

Python 3: [Download link](https://www.python.org/downloads/)

Python `discord.py` module: To install it, enter the following command in cmd or a terminal:

```
pip install discord.py
```

## Notes

- Why is this code so terrible? It was originally generated almost entirely by chatgpt. The original "creator" was begging anyone for help fixing the code. I did not clean up this code at all and strictly fixed the function. This means the general flow was auto generated. It's about as flexible as a rock.

- Threads with <50 messages archive at 1 hour, threads with <100 messages archive at 1.5 hours, threads with <150 messages archive at 2 hours, threads with <200 messages archive at 3 hours.

- The bot will send a warning 10 minutes before archiving the thread.

- Threads with a message sent within the last 5 minutes will not archive. If a thread is scheduled to archive and a message is sent in the last 5 minutes the bot will send a message stating the timer is paused. If another message is sent while the timer is paused, the timer will not unpause until there has been at least 5 minutes of inactivity. After 5 minutes of inactivity, the bot will send a message stating the timer has resumed, the timer will resume, and the thread will archive after another 5 minutes of no activity.
