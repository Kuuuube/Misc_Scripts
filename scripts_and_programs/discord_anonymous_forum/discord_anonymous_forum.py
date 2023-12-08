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

# Bot message configuration
dm_error_message = "ＥＲＲＯＲ： Use /p to post. It is also recommended you unfollow the thread. \n書き込むには/pを使用してください。また、スレッドへのフォローを解除することをおすすめします。"
anon_title_prefix = " 名無しさん@TMW.bbs (ID: "
anon_title_suffix = ")"
interaction_confirmation_prefix = "書き込みが終わりました。 ["
interaction_confirmation_suffix = "]\n\nこのメッセージを非表示にすることができます。"
attachment_prefix = "\n\n**Attachment:**\n"

# General configuration
logging_enabled = True
enabled_guild_ids = [your_guild_ids_here]
forum_channel_ids = [your_forum_channel_ids_here]


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
    current_day = datetime.datetime.utcnow().strftime("%Y%m%d")
    user_id = hashlib.sha256((str(discord_user_id) + random_salt + current_day).encode()).hexdigest()[:9]

    if not logging_enabled:
        return user_id

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

    if hasattr(message.channel, "parent_id") and message.channel.parent_id in forum_channel_ids:
        if message.channel.id in open_threads:
            await replace_message(message)
        else:
            channel = bot.get_channel(message.channel.parent_id)
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
        if message.attachments:
            files = []
            for attachment in message.attachments:
                if attachment.content_type.startswith("image"):
                    files.append(await attachment.to_file())
                    break #only first image is used
            await message.channel.delete()
            embed = nextcord.Embed(title = "001" + anon_title_prefix + user_id + anon_title_suffix, description = message.content)
            if files:
                embed.set_image("attachment://" + files[0].filename)
            await channel.create_thread(name = message.channel.name, embed = embed, files = files)
        else:
            await message.channel.delete()
            await channel.create_thread(name = message.channel.name, embed = nextcord.Embed(title = "001" + anon_title_prefix + user_id + anon_title_suffix, description = message.content))
    except Exception as e:
        print("Replace thread failed: ", e)

async def replace_message(message):
    try:
        await message.delete()
        # Send DM
        await message.author.send(dm_error_message)
        # If you would prefer users to be able to send messages normally and have their messages replaced, uncomment the below lines
        # You may also want to comment out the above line that sends the DM if you are not deleting normal messages
        #
        #if message.attachments:
        #    await message.channel.send(embed = nextcord.Embed(description = message.content), files = [await attachment.to_file() for attachment in message.attachments])
        #else:
        #    await message.channel.send(message.content)
    except Exception as e:
        print(f'An error occurred: {e}')

@bot.slash_command(name = "p", guild_ids = enabled_guild_ids)
async def post_command(interaction: nextcord.Interaction, message: str, attachment: nextcord.Attachment = nextcord.SlashOption(required = False)):
    user_id = get_id(interaction.user.id)

    if hasattr(interaction.channel, "parent_id") and interaction.channel.parent_id in forum_channel_ids:
        await interaction.send(interaction_confirmation_prefix + str(round(bot.latency, 6)) + interaction_confirmation_suffix, ephemeral=True)
        if attachment:
            await send_attachment_message(interaction.channel, message.replace("\\n", "\n"), user_id, attachment)
        else:
            await send_message(interaction.channel, message.replace("\\n", "\n"), user_id)
    else:
        await interaction.send("You cannot use this command here", ephemeral=True)

async def send_message(channel, message, user_id):
    thread_length = 1
    async for _ in channel.history(limit=None):
        thread_length += 1
    try:
        await channel.send(embed = nextcord.Embed(title = format(thread_length, "03") + anon_title_prefix + user_id + anon_title_suffix, description = message))
    except Exception as e:
        print("Send message failed: ", e)

async def send_attachment_message(channel, message, user_id, attachment):
    thread_length = 1
    async for _ in channel.history(limit=None):
        thread_length += 1
    try:
        embed = nextcord.Embed(title = format(thread_length, "03") + anon_title_prefix + user_id + anon_title_suffix, description = message)
        if attachment.content_type.startswith("image"):
            embed.set_image(attachment.proxy_url)
        else:
            embed.description += attachment_prefix + "[" + attachment.filename + "](" + attachment.proxy_url.replace("media.discordapp.net", "cdn.discordapp.com") + ") (" + attachment.content_type.rsplit(";")[0] + ")"
        await channel.send(embed = embed)
    except Exception as e:
        print("Send message failed: ", e)

@bot.slash_command(name = "check_id", guild_ids = enabled_guild_ids, default_member_permissions = nextcord.Permissions(administrator=True))
async def check_id(interaction: nextcord.Interaction, message_id: str):
    for i, message_id_list in enumerate(userids.values()):
        if message_id in message_id_list:
            await interaction.send("Message ID (" + str(message_id) + ") matches User ID (" + str(list(userids.keys())[i]) + ")", ephemeral=True)
            return

    await interaction.send("Message ID (" + str(message_id) + ") does not match any logged User ID", ephemeral=True)

# This command is the same as the /p (post command) but it accepts a messageid to add as a reply
# Uncomment all below lines to enable this commmand
#
#@bot.slash_command(name = "r", guild_ids = enabled_guild_ids)
#async def reply_command(interaction: nextcord.Interaction, message: str, reply: str):
#    if hasattr(interaction.channel, "parent_id") and interaction.channel.parent_id in forum_channel_ids:
#        reference = await get_message_reference(interaction.channel.id, reply)
#        if not reference:
#            await interaction.send("Failed to set reply reference", ephemeral=True)
#            return
#        await interaction.send("書き込みが終わりました。 [" + str(round(bot.latency, 6)) + "]\n\nこのメッセージを非表示にすることができます。", ephemeral=True)
#        await send_reply(interaction.channel, message.replace("\\n", "\n"), reply)
#    else:
#        await interaction.send("You cannot use this command here", ephemeral=True)
#
#async def send_reply(channel, message, reply):
#    try:
#        await channel.send(embed = nextcord.Embed(description = message), reference = nextcord.MessageReference(channel_id = channel.id, message_id = int(reply)))
#    except Exception as e:
#        print(f'An error occurred: {e}')
#
#async def get_message_reference(channel_id, message_id_str):
#    try:
#        return nextcord.MessageReference(channel_id = channel_id, message_id = int(message_id_str))
#    except Exception:
#        return None

@bot.event
async def on_ready():
    print("Logged in as: " + str(bot.user))

bot.run(open(".env", "r", encoding="UTF-8").read().split("=", 1)[1].strip())
