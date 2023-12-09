import nextcord
import nextcord.ext.commands
import datetime
import hashlib
import secrets
import json

intents = nextcord.Intents.default()
intents.messages = True
intents.message_content = True

bot = nextcord.ext.commands.Bot(intents = intents)

post_slowmode_timers = {}
last_post = {}
open_threads = []
random_salt = secrets.token_hex(256)

def read_json(filename):
    try:
        with open (filename, "r") as json_file:
            userids = json.load(json_file)
            return userids
    except Exception as e:
        print("Failed to read json " + str(filename) + ": ", e)
        return {}

def write_json(filename, json_object):
    try:
        with open (filename, "w") as json_file:
            json_file.write(json.dumps(json_object, indent=4))
    except Exception as e:
        print("Failed to write json " + str(filename) + ": ", e)

userids = read_json("userid_log.json")
settings = read_json("settings.json")

def get_id(discord_user_id):
    current_day = datetime.datetime.utcnow().strftime("%Y%m%d")
    user_id = hashlib.sha256((str(discord_user_id) + random_salt + current_day).encode()).hexdigest()[:9]

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
    except Exception as e:
        print(e)

    return user_id

@bot.event
async def on_message(message):
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
            embed = nextcord.Embed(title = "001" + settings["bot_embed_title_prefix"] + user_id + settings["bot_embed_title_suffix"], description = message.content)
            if files:
                embed.set_image("attachment://" + files[0].filename)
            await channel.create_thread(name = message.channel.name, embed = embed, files = files)
        else:
            await message.channel.delete()
            await channel.create_thread(name = message.channel.name, embed = nextcord.Embed(title = "001" + settings["bot_embed_title_prefix"] + user_id + settings["bot_embed_title_suffix"], description = message.content))
    except Exception as e:
        print("Replace thread failed: ", e)

async def replace_message(message):
    try:
        await message.delete()
        await message.author.send(settings["bot_dm_on_normal_message"]) # Send DM
    except Exception as e:
        print(f'An error occurred: {e}')

@bot.slash_command(name = "p", guild_ids = settings["enabled_guild_ids"])
async def post_command(interaction: nextcord.Interaction, message: str, attachment: nextcord.Attachment = nextcord.SlashOption(required = False)):
    for role in interaction.user.roles:
        if role.id in settings["blacklisted_roles"] or role.name in settings["blacklisted_roles"]:
            await interaction.send(settings["blacklisted_message"], ephemeral=True)
            return

    if await spam_check(interaction, interaction.user.id, message):
        return

    user_id = get_id(interaction.user.id)

    if hasattr(interaction.channel, "parent_id") and interaction.channel.parent_id in settings["forum_channel_ids"]:
        await interaction.send(settings["interaction_confirmation_prefix"] + str(round(bot.latency, 6)) + settings["interaction_confirmation_suffix"], ephemeral=True)
        if attachment:
            await send_attachment_message(interaction.channel, message.replace("\\n", "\n"), user_id, attachment)
        else:
            await send_message(interaction.channel, message.replace("\\n", "\n"), user_id)
    else:
        await interaction.send(settings["wrong_channel_message"], ephemeral=True)

async def send_message(channel, message, user_id):
    thread_length = 1
    async for _ in channel.history(limit=None):
        thread_length += 1
    try:
        await channel.send(embed = nextcord.Embed(title = format(thread_length, "03") + settings["bot_embed_title_prefix"] + user_id + settings["bot_embed_title_suffix"], description = message))
    except Exception as e:
        print("Send message failed: ", e)

async def send_attachment_message(channel, message, user_id, attachment):
    thread_length = 1
    async for _ in channel.history(limit=None):
        thread_length += 1
    try:
        embed = nextcord.Embed(title = format(thread_length, "03") + settings["bot_embed_title_prefix"] + user_id + settings["bot_embed_title_suffix"], description = message)
        if attachment.content_type.startswith("image"):
            embed.set_image(attachment.proxy_url)
        else:
            embed.description += settings["attachment_prefix"] + "[" + attachment.filename + "](" + attachment.proxy_url.replace("media.discordapp.net", "cdn.discordapp.com") + ") (" + attachment.content_type.rsplit(";")[0] + ")"
        await channel.send(embed = embed)
    except Exception as e:
        print("Send message failed: ", e)

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

@bot.slash_command(name = "check_id", guild_ids = settings["enabled_guild_ids"], default_member_permissions = nextcord.Permissions(administrator=True))
async def check_id(interaction: nextcord.Interaction, message_id: str):
    for i, message_id_list in enumerate(userids.values()):
        if message_id in message_id_list:
            await interaction.send("Message ID (" + str(message_id) + ") matches User ID (" + str(list(userids.keys())[i]) + ")", ephemeral=True)
            return

    await interaction.send("Message ID (" + str(message_id) + ") does not match any logged User ID", ephemeral=True)

@bot.event
async def on_ready():
    print("Logged in as: " + str(bot.user))

bot.run(open(".env", "r", encoding="UTF-8").read().split("=", 1)[1].strip())
