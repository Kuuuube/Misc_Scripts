import nextcord
import nextcord.ext.commands
import datetime
import json
import sys
import time
import traceback

intents = nextcord.Intents.default()
intents.guilds = True
intents.messages = True

bot = nextcord.ext.commands.Bot(intents = intents)

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

def validate_settings(json_dict):
    class JSONValidationException(Exception):
        pass

    expected_fields = [("enabled_guild_ids", list, [int]),
                       ("forum_channel_ids", list, [int]),
                       ("max_threads", int),
                       ("loop_timer_seconds", int)]
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

settings = read_json("settings.json")
if not validate_settings(settings):
    #Slow Blink: `\033[5m`, Bright Red: `\033[91m`, Underline: `\033[4m`, Bold: `\033[1m`, Invert FG/BG: `\033[7m`, Reset: `\033[0m`
    for _ in range(2):
        sys.stdout.write("\033[5m\033[91m\033[4m\033[1m\033[7m" + "!!!FATAL: INITIAL SETTINGS VALIDATION FAILED!!! EXITING" + "\033[0m" + "\r")
        time.sleep(0.1)
        sys.stdout.write("\033[5m\033[91m\033[4m\033[1m" + "!!!FATAL: INITIAL SETTINGS VALIDATION FAILED!!! EXITING" + "\033[0m" + "\r")
        time.sleep(0.1)
    sys.exit()

async def find_active_threads(threads):
    new_threads = []
    for thread in threads:
        if not thread.locked:
            last_message = await thread.fetch_message(thread.last_message_id)
            if hasattr(last_message, "created_at"):
                new_threads.append((thread, last_message.created_at))
            else:
                error_log("Failed to fetch thread last message")
    
    new_threads.sort(key=lambda x: x[1])
    return new_threads

async def archive_threads():
    while True:
        try:
            base_log("Started processing threads")
            for channel_id in settings["forum_channel_ids"]:
                channel = bot.get_channel(channel_id)
                timestamped_threads = await find_active_threads(channel.threads)
                while len(timestamped_threads) > settings["max_threads"]:
                    await timestamped_threads[0][0].edit(archived = True, locked = True)
                    timestamped_threads.pop(0)
                    base_log("Archived thread: " + str(timestamped_threads[0][0]))
            base_log("Finished processing threads")
        except Exception:
            error_log("Processing threads failed: ", traceback.format_exc())
        time.sleep(settings["loop_timer_seconds"])

@bot.slash_command(name = "reload_settings", guild_ids = settings["enabled_guild_ids"], default_member_permissions = nextcord.Permissions(administrator=True))
async def reload_settings(interaction: nextcord.Interaction):
    global settings
    new_settings = read_json("settings.json")
    if new_settings and validate_settings(new_settings):
        settings = new_settings
        base_log("Settings reloaded successfully")
        await interaction.send("Settings reloaded successfully. Note: Changes to enabled_guild_ids require a restart.", ephemeral=True)
    else:
        base_log("Settings file parsing or validation failed")
        await interaction.send("Settings file parsing or validation failed. Settings will not be reloaded.", ephemeral=True)

@bot.event
async def on_ready():
    base_log(f'Logged in as {bot.user}')
    bot.loop.create_task(archive_threads())

bot.run(open(".env", "r", encoding="UTF-8").read().split("=", 1)[1].strip())
