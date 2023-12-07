import nextcord
import nextcord.ext.commands
import time
import datetime
import random
import hashlib
import secrets
import json
import os

intents = nextcord.Intents.default()
intents.messages = True
intents.message_content = True

bot = nextcord.ext.commands.Bot(intents = intents)

# Replace with your forum channel ID
FORUM_CHANNEL_ID = your_forum_channel_id_here

open_threads = []
random_salt = secrets.token_hex(256)

def read_json(filename):
    try:
        with open (filename, "r") as json_file:
            userids = json.load(json_file)
            return userids
    except Exception as e:
        print("Failed to read json: ", e)
        return {}

def write_json(filename, json_object):
    try:
        with open (filename, "w") as json_file:
            json_file.write(json.dumps(json_object, indent=4))
    except Exception as e:
        print("Failed to write json: ", e)

userids = read_json("userid_log.json")

def get_id(discord_user_id):
    user_id = hashlib.sha256((str(discord_user_id) + random_salt).encode()).hexdigest()[:9]

    write_file = False

    try:
        if str(discord_user_id) in userids.keys():
            if user_id not in userids[str(discord_user_id)]:
                userids[str(discord_user_id)].append(user_id)
                write_file = True
        else:
            userids[str(discord_user_id)] = [user_id]
            write_file = True

        if write_file:
            write_json("userid_log.json", userids)
    except Exception as e:
        print(e)

    return user_id

@bot.event
async def on_message(message):
    if message.author == bot.user or message.author.bot:
        return

    if hasattr(message.channel, "parent_id") and message.channel.parent_id == FORUM_CHANNEL_ID:
        if message.channel.id in open_threads:
            await replace_message(message)
        else:
            channel = bot.get_channel(FORUM_CHANNEL_ID)
            thread = channel.get_thread(message.channel.id)
            first_message = await thread.history(limit = 1, oldest_first = True).__anext__()
            if message.id == first_message.id:
                await replace_thread(channel, message)
            else:
                await replace_message(message)
                open_threads.append(message.channel.id)

async def replace_thread(channel, message):
    try:
        user_id = get_id(message.author.id)
        await message.channel.delete()
        if message.attachments:
            await channel.create_thread(name = message.channel.name, embed = nextcord.Embed(title = "001 名無しさん@TMW.bbs (ID: " + user_id + ")", description = message.content), files = [await attachment.to_file() for attachment in message.attachments])
        else:
            await channel.create_thread(name = message.channel.name, embed = nextcord.Embed(title = "001 名無しさん@TMW.bbs (ID: " + user_id + ")", description = message.content))
    except Exception as e:
        print(f'An error occurred: {e}')

async def replace_message(message):
    try:
        await message.delete()
        #send dm
        await message.author.send("ＥＲＲＯＲ： Use /p to post. It is also recommended you unfollow the thread. \n書き込むには/pを使用してください。また、スレッドへのフォローを解除することをおすすめします。")
        #if message.attachments:
        #    await message.channel.send(embed = nextcord.Embed(description = message.content), files = [await attachment.to_file() for attachment in message.attachments])
        #else:
        #    await message.channel.send(message.content)
    except Exception as e:
        print(f'An error occurred: {e}')

@bot.slash_command(name = "p")
async def post_command(interaction: nextcord.Interaction, message: str):
    user_id = get_id(interaction.user.id)

    if hasattr(interaction.channel, "parent_id") and interaction.channel.parent_id == FORUM_CHANNEL_ID:
        await interaction.send("書き込みが終わりました。 [" + str(round(bot.latency, 6)) + "]\n\nこのメッセージを非表示にすることができます。", ephemeral=True)
        await send_message(interaction.channel, message.replace("\\n", "\n"), user_id)
    else:
        await interaction.send("You cannot use this command here", ephemeral=True)

@bot.slash_command(name = "r")
async def reply_command(interaction: nextcord.Interaction, message: str):
    await post_command(interaction, message)
#async def reply_command(interaction: nextcord.Interaction, message: str, reply: str):
    #if hasattr(interaction.channel, "parent_id") and interaction.channel.parent_id == FORUM_CHANNEL_ID:
    #    reference = await get_message_reference(interaction.channel.id, reply)
    #    if not reference:
    #        await interaction.send("Failed to set reply reference", ephemeral=True)
    #        return
    #    await interaction.send("書き込みが終わりました。 [" + str(round(bot.latency, 6)) + "]\n\nこのメッセージを非表示にすることができます。", ephemeral=True)
    #    await send_reply(interaction.channel, message.replace("\\n", "\n"), reply)
    #else:
    #    await interaction.send("You cannot use this command here", ephemeral=True)

async def send_message(channel, message, user_id):
    thread_length = 1
    async for _ in channel.history(limit=None):
        thread_length += 1
    try:
        await channel.send(embed = nextcord.Embed(title = format(thread_length, "03") + " 名無しさん@TMW.bbs (ID: " + user_id + ")", description = message))
    except Exception as e:
        print(f'An error occurred: {e}')

#async def send_reply(channel, message, reply):
#    try:
#        await channel.send(embed = nextcord.Embed(description = message), reference = nextcord.MessageReference(channel_id = channel.id, message_id = int(reply)))
#    except Exception as e:
#        print(f'An error occurred: {e}')

async def get_message_reference(channel_id, message_id_str):
    try:
        return nextcord.MessageReference(channel_id = channel_id, message_id = int(message_id_str))
    except Exception:
        return None

@bot.event
async def on_ready():
    print(f'Logged in as {bot.user}')

bot.run(open(".env", "r", encoding="UTF-8").read().split("=", 1)[1].strip())
