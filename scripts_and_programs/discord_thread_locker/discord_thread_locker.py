import discord
from datetime import datetime, timedelta, timezone
import asyncio

intents = discord.Intents.default()
intents.guilds = True
intents.messages = True  # Ensure this intent is enabled for message history

client = discord.Client(intents=intents)

# List of specific channel IDs to check
eng_channel_ids = []
jp_channel_ids = []

async def archive_threads_with_notifications():
    thread_timers = {}
    notification_sent = []
    grace_notification_sent = []

    while True:
        for channel_id in (eng_channel_ids + jp_channel_ids):
            channel = client.get_channel(channel_id)
            if channel:
                print(f"Processing Channel: {channel.name} (ID: {channel.id})")  # Debug print
                threads = channel.threads
                for thread in threads:
                    print(f"Processing thread: {thread.name} (ID: {thread.id})")  # Debug print
                    if thread.locked:
                        print("Thread locked, skipping")
                        continue

                    # Traditional loop for message history
                    replies = 0
                    async for message in thread.history(limit=None):
                        replies += 1

                    first_message = None
                    async for message in thread.history(limit=1, oldest_first=True):
                        first_message = message

                    if thread.id not in thread_timers:
                        if first_message:
                            thread_timers[thread.id] = first_message.created_at
                        else:
                            thread_timers[thread.id] = datetime.now(timezone.utc)

                    elapsed_time = datetime.now(timezone.utc) - thread_timers[thread.id]
                    print("Elapsed Time: " + str(elapsed_time))

                    last_message = None
                    async for message in thread.history(limit=1):
                        last_message = message
                    print("Last message sent: " + str(datetime.now(timezone.utc) - last_message.created_at))

                    # Adjust archival time based on reply count
                    print("Replies: " + str(replies))
                    if replies < 50:
                        max_duration = timedelta(hours=1)
                    elif replies < 100:
                        max_duration = timedelta(hours=1, minutes=30)
                    elif replies < 200:
                        max_duration = timedelta(hours=2)
                    else:
                        max_duration = timedelta(hours=3)

                    print("Max Duration: " + str(max_duration))

                    # Archiving the thread
                    if elapsed_time >= max_duration and not datetime.now(timezone.utc) - last_message.created_at < timedelta(minutes=5):
                        archive_message = "This thread is archived. You cannot reply anymore."
                        if thread.parent_id in jp_channel_ids:
                            archive_message = "このスレッドはアーカイブされています。もう書き込むことはできません。"
                        await thread.send(archive_message)
                        await thread.edit(archived=True, locked=True)
                        del thread_timers[thread.id]  # Remove thread from tracking
                        if thread.id in notification_sent:
                            notification_sent.remove(thread.id)
                        continue

                    # Send a 10-minute warning message if applicable
                    if elapsed_time >= (max_duration - timedelta(minutes=10)) and thread.id not in notification_sent:
                        notification_message = "This thread will get archived in 10 minutes. You can prolong its life by posting in it more."
                        if thread.parent_id in jp_channel_ids:
                            notification_message = "このスレッドは１０分以内にアーカイブされます。さらに書き込むことで長持ちできます。"
                        await thread.send(notification_message)
                        notification_sent.append(thread.id)
                        continue

                    # Check if there is recent activity and handle timer pause
                    if elapsed_time >= (max_duration - timedelta(minutes=5)) and last_message and datetime.now(timezone.utc) - last_message.created_at < timedelta(minutes=5) and not last_message.author.bot and thread.id not in grace_notification_sent:
                        thread_timers[thread.id] += timedelta(minutes=5)
                        print("Timer stopped subtracting time: " + str(datetime.now(timezone.utc) - thread_timers[thread.id]))
                        pause_message = "Thread Stopper has stopped!"
                        if thread.parent_id in jp_channel_ids:
                            pause_message = "Thread Stopperが止まりました！"
                        await thread.send(pause_message)
                        grace_notification_sent.append(thread.id)
                        continue

                    # Handle timer resuming
                    elif elapsed_time >= (max_duration - timedelta(minutes=5)) and last_message and datetime.now(timezone.utc) - last_message.created_at >= timedelta(minutes=5) and thread.id in grace_notification_sent:
                        thread_timers[thread.id] += timedelta(minutes=5)
                        print("Timer started subtracting time: " + str(datetime.now(timezone.utc) - thread_timers[thread.id]))
                        resume_message = "Thread Stopper has started!"
                        if thread.parent_id in jp_channel_ids:
                            resume_message = "Thread Stopperが動きました！"
                        await thread.send(resume_message)
                        if thread.id in grace_notification_sent:
                            grace_notification_sent.remove(thread.id)

        await asyncio.sleep(60)  # Check every minute

@client.event
async def on_ready():
    print(f'Logged in as {client.user}')  # Debug print
    client.loop.create_task(archive_threads_with_notifications())

client.run(open(".env", "r", encoding="UTF-8").read().split("=", 1)[1].strip())
