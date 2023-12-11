import nextcord
import nextcord.ext.commands
import datetime
import hashlib
import secrets
import json
import sys
import time
import traceback
import logging

intents = nextcord.Intents.default()
intents.messages = True
intents.message_content = True

bot = nextcord.ext.commands.Bot(intents = intents)

post_slowmode_timers = {}
last_post = {}
open_threads = []
random_salt = secrets.token_hex(256)

def nextcord_debug_log():
    logger = logging.getLogger("nextcord")
    logger.setLevel(logging.DEBUG)
    handler = logging.FileHandler("nextcord.log", "a", encoding = "utf-8")
    handler.setFormatter(logging.Formatter("%(asctime)s:%(levelname)s:%(name)s: %(message)s"))
    logger.addHandler(handler)

def error_log(message, error = ""):
    try:
        utc_time = datetime.datetime.utcnow().strftime("%Y-%m-%d_%H-%M-%S")
        with open ("error_log.log", "a", encoding="utf8") as log_file:
            log_file.write(utc_time + ", " + str(message).replace("\r", r"\r").replace("\n", r"\n") + ", " + str(error).replace("\r", r"\r").replace("\n", r"\n") + "\n")
        print(message)
        print(error)
    except Exception:
        try:
            print("Could not write to error log:")
            print(traceback.format_exc())
        except Exception:
            pass

def base_log(message):
    try:
        utc_time = datetime.datetime.utcnow().strftime("%Y-%m-%d_%H-%M-%S")
        with open ("base_log.log", "a", encoding="utf8") as log_file:
            log_file.write(utc_time + ", " + str(message).replace("\r", r"\r").replace("\n", r"\n") + "\n")
        print(message)
    except Exception:
        try:
            print("Could not write to base log:")
            print(traceback.format_exc())
        except Exception:
            pass

def read_json(filename):
    try:
        with open (filename, "r") as json_file:
            userids = json.load(json_file)
            return userids
    except Exception:
        error_log("Failed to read json " + str(filename) + ": ", traceback.format_exc())
        return {}

def write_json(filename, json_object):
    try:
        with open (filename, "w") as json_file:
            json_file.write(json.dumps(json_object, indent=4))
    except Exception:
        error_log("Failed to write json " + str(filename) + ": ", traceback.format_exc())

def validate_settings(json_dict):
    class JSONValidationException(Exception):
        pass

    expected_fields = [("enabled_guild_ids", list, [int]),
                       ("forum_channel_ids", list, [int]),
                       ("blacklisted_roles", list, [int, str]),
                       ("post_slowmode", int),
                       ("restrict_duplicate_messages", bool),
                       ("per_day_ids", bool),
                       ("per_thread_ids", bool),
                       ("logging_enabled", bool),
                       ("bot_dm_on_normal_message", str),
                       ("bot_embed_title_prefix", str),
                       ("bot_embed_title_suffix", str),
                       ("interaction_confirmation_prefix", str),
                       ("interaction_confirmation_suffix", str),
                       ("attachment_prefix", str),
                       ("blacklisted_message", str),
                       ("wrong_channel_message", str),
                       ("post_slowmode_error_message_prefix", str),
                       ("post_slowmode_error_message_suffix", str),
                       ("restrict_duplicate_messages_error_message", str)]
    try:
        for expected_field in expected_fields:
            if type(json_dict[expected_field[0]]) != expected_field[1]:
                raise JSONValidationException(expected_field[0] + " expected type `" + str(expected_field[1].__name__) + "` but found type `" + str(type(json_dict[expected_field[0]]).__name__) + "`")
            if expected_field[1] == list:
                for item in json_dict[expected_field[0]]:
                    if type(item) not in expected_field[2]:
                        raise JSONValidationException(expected_field[0] + " expected type `" + str([expected_type.__name__ for expected_type in expected_field[2]]).replace("'", "")  + "` but found type `[" + str(type(item).__name__) + "]`")
        return True
    except Exception:
        error_log("Settings validation failed: ", traceback.format_exc())

userids = read_json("userid_log.json")
settings = read_json("settings.json")
if not validate_settings(settings):
    #Slow Blink: `\033[5m`, Bright Red: `\033[91m`, Underline: `\033[4m`, Bold: `\033[1m`, Invert FG/BG: `\033[7m`, Reset: `\033[0m`
    for _ in range(2):
        sys.stdout.write("\033[5m\033[91m\033[4m\033[1m\033[7m" + "!!!FATAL: INITIAL SETTINGS VALIDATION FAILED!!! EXITING" + "\033[0m" + "\r")
        time.sleep(0.1)
        sys.stdout.write("\033[5m\033[91m\033[4m\033[1m" + "!!!FATAL: INITIAL SETTINGS VALIDATION FAILED!!! EXITING" + "\033[0m" + "\r")
        time.sleep(0.1)
    sys.exit()

def get_id(discord_user_id, channel_id):
    hashstring = str(discord_user_id) + random_salt

    if settings["per_day_ids"]:
        current_day = datetime.datetime.utcnow().strftime("%Y%m%d")
        hashstring += current_day
    if settings["per_thread_ids"]:
        hashstring += str(channel_id)

    user_id = hashlib.sha256((hashstring).encode()).hexdigest()[:9]

    if not settings["logging_enabled"]:
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
    except Exception:
        error_log("Logging message id failed: ", traceback.format_exc())

    return user_id

@bot.event
async def on_message(message):
    try:
        if message.author == bot.user or message.author.bot:
            return

        if hasattr(message.channel, "parent_id") and message.channel.parent_id in settings["forum_channel_ids"]:
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
    except Exception:
        error_log("On_message failed", traceback.format_exc())

async def replace_thread(channel, message):
    if settings["per_thread_ids"]:
        await edit_replace_thread(channel, message)
    else:
        await direct_replace_thread(channel, message)

async def direct_replace_thread(channel, message):
    try:
        user_id = get_id(message.author.id, channel.id)
        if message.attachments:
            files = []
            for attachment in message.attachments:
                if attachment.content_type.startswith("image"):
                    files.append(await attachment.to_file())
                    break #only first image is used
            await message.channel.delete()
            embed = nextcord.Embed(title = "001" + settings["bot_embed_title_prefix"] + user_id + settings["bot_embed_title_suffix"], description = message.content)
            if files:
                embed.set_image("attachment://" + files[0].filename)
            await channel.create_thread(name = message.channel.name, embed = embed, files = files)
        else:
            await message.channel.delete()
            await channel.create_thread(name = message.channel.name, embed = nextcord.Embed(title = "001" + settings["bot_embed_title_prefix"] + user_id + settings["bot_embed_title_suffix"], description = message.content))
    except Exception:
        error_log("Replace thread failed: ", traceback.format_exc())

async def edit_replace_thread(channel, message):
    try:
        if message.attachments:
            files = []
            for attachment in message.attachments:
                if attachment.content_type.startswith("image"):
                    files.append(await attachment.to_file())
                    break #only first image is used
            await message.channel.delete()
            embed = nextcord.Embed(title = "001" + settings["bot_embed_title_prefix"] + "000000000" + settings["bot_embed_title_suffix"], description = message.content)
            if files:
                embed.set_image("attachment://" + files[0].filename)
            thread = await channel.create_thread(name = message.channel.name, embed = embed, files = files)
            user_id = get_id(message.author.id, thread.id)
            embed.title = "001" + settings["bot_embed_title_prefix"] + user_id + settings["bot_embed_title_suffix"]
            await thread.last_message.edit(embed = embed)
        else:
            await message.channel.delete()
            embed = nextcord.Embed(title = "001" + settings["bot_embed_title_prefix"] + "000000000" + settings["bot_embed_title_suffix"], description = message.content)
            thread = await channel.create_thread(name = message.channel.name, embed = embed)
            user_id = get_id(message.author.id, thread.id)
            embed.title = "001" + settings["bot_embed_title_prefix"] + user_id + settings["bot_embed_title_suffix"]
            await thread.last_message.edit(embed = embed)
    except Exception:
        error_log("Replace thread failed: ", traceback.format_exc())

async def replace_message(message):
    try:
        await message.delete()
        await message.author.send(settings["bot_dm_on_normal_message"]) # Send DM
    except Exception:
        error_log("Replace message failed: ", traceback.format_exc())

@bot.slash_command(name = "p", guild_ids = settings["enabled_guild_ids"])
async def post_command(interaction: nextcord.Interaction, message: str, attachment: nextcord.Attachment = nextcord.SlashOption(required = False)):
    try:
        for role in interaction.user.roles:
            if role.id in settings["blacklisted_roles"] or role.name in settings["blacklisted_roles"]:
                await interaction.send(settings["blacklisted_message"], ephemeral=True)
                return

        if await spam_check(interaction, interaction.user.id, message):
            return

        user_id = get_id(interaction.user.id, interaction.channel.id)

        if hasattr(interaction.channel, "parent_id") and interaction.channel.parent_id in settings["forum_channel_ids"]:
            await interaction.send(settings["interaction_confirmation_prefix"] + str(round(bot.latency, 6)) + settings["interaction_confirmation_suffix"], ephemeral=True)
            if attachment:
                await send_attachment_message(interaction.channel, message.replace("\\n", "\n"), user_id, attachment)
            else:
                await send_message(interaction.channel, message.replace("\\n", "\n"), user_id)
        else:
            await interaction.send(settings["wrong_channel_message"], ephemeral=True)
    except Exception:
        error_log("Post command failed: ", traceback.format_exc())

async def send_message(channel, message, user_id):
    try:
        thread_length = 1
        async for _ in channel.history(limit=None):
            thread_length += 1
        await channel.send(embed = nextcord.Embed(title = format(thread_length, "03") + settings["bot_embed_title_prefix"] + user_id + settings["bot_embed_title_suffix"], description = message))
    except Exception:
        error_log("Send message failed: ", traceback.format_exc())

async def send_attachment_message(channel, message, user_id, attachment):
    try:
        thread_length = 1
        async for _ in channel.history(limit=None):
            thread_length += 1
        embed = nextcord.Embed(title = format(thread_length, "03") + settings["bot_embed_title_prefix"] + user_id + settings["bot_embed_title_suffix"], description = message)
        if attachment.content_type.startswith("image"):
            embed.set_image(attachment.proxy_url)
        else:
            embed.description += settings["attachment_prefix"] + "[" + attachment.filename + "](" + attachment.proxy_url.replace("media.discordapp.net", "cdn.discordapp.com") + ") (" + attachment.content_type.rsplit(";")[0] + ")"
        await channel.send(embed = embed)
    except Exception:
        error_log("Send attachment message failed: ", traceback.format_exc())

async def spam_check(interaction, discord_user_id, current_post_content):
    # post slowmode check
    if settings["post_slowmode"] > 0:
        if discord_user_id in post_slowmode_timers.keys() and datetime.datetime.utcnow() - post_slowmode_timers[discord_user_id] < datetime.timedelta(seconds = settings["post_slowmode"]):
            await interaction.send(settings["post_slowmode_error_message_prefix"] + str(settings["post_slowmode"]) + settings["post_slowmode_error_message_suffix"], ephemeral=True)
            return True
        else:
            post_slowmode_timers[discord_user_id] = datetime.datetime.utcnow()

    # restrict duplicate messages check
    if settings["restrict_duplicate_messages"]:
        if discord_user_id in last_post.keys() and last_post[discord_user_id] == current_post_content:
            await interaction.send(settings["restrict_duplicate_messages_error_message"], ephemeral=True)
            return True
        else:
            last_post[discord_user_id] = current_post_content

    return False

@bot.slash_command(name = "rename_thread", guild_ids = settings["enabled_guild_ids"], default_member_permissions = nextcord.Permissions(manage_threads=True))
async def rename_thread(interaction: nextcord.Interaction, new_thread_name: str):
    if hasattr(interaction.channel, "parent_id") and interaction.channel.parent_id in settings["forum_channel_ids"]:
        try:
            await interaction.channel.edit(name = new_thread_name)
            await interaction.send("Thread renamed", ephemeral=True)
        except Exception:
            error_log("Failed to rename thread: ", traceback.format_exc())
            await interaction.send("Failed to rename thread", ephemeral=True)
    else:
        await interaction.send(settings["wrong_channel_message"], ephemeral=True)

@bot.slash_command(name = "check_id", guild_ids = settings["enabled_guild_ids"], default_member_permissions = nextcord.Permissions(administrator=True))
async def check_id(interaction: nextcord.Interaction, message_id: str):
    for i, message_id_list in enumerate(userids.values()):
        if message_id in message_id_list:
            await interaction.send("Message ID (" + str(message_id) + ") matches User ID (" + str(list(userids.keys())[i]) + ")", ephemeral=True)
            return

    await interaction.send("Message ID (" + str(message_id) + ") does not match any logged User ID", ephemeral=True)

@bot.slash_command(name = "reload_settings", guild_ids = settings["enabled_guild_ids"], default_member_permissions = nextcord.Permissions(administrator=True))
async def reload_settings(interaction: nextcord.Interaction):
    global settings
    new_settings = read_json("settings.json")
    if new_settings and validate_settings(new_settings):
        settings = new_settings
        base_log("Settings reloaded successfully")
        await interaction.send("Settings reloaded successfully. Note: Changes to enabled_guild_ids require a restart.", ephemeral=True)
    else:
        error_log("Settings file parsing or validation failed")
        await interaction.send("Settings file parsing or validation failed. Settings will not be reloaded.", ephemeral=True)

@bot.event
async def on_ready():
    base_log("Logged in as: " + str(bot.user))

nextcord_debug_log()
bot.run(open(".env", "r", encoding="UTF-8").read().split("=", 1)[1].strip())
