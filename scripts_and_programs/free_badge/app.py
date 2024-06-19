import nextcord
import nextcord.ext.commands
import os
import dotenv
import datetime
import random

dotenv.load_dotenv()

bot = nextcord.ext.commands.Bot()

@bot.event
async def on_ready():
    print('Logged in as ' + bot.user.name + ' (' + str(bot.user.id) + ')')

@bot.slash_command(description="Free Badge")
async def free_badge(interaction: nextcord.Interaction):
    user_id = str(interaction.user.id)
    user_name = str(interaction.user.name) + "#" + str(interaction.user.discriminator)
    command_name = str(interaction.application_command.name)
    await interaction.send(fetch_command_info(), ephemeral=True)
    log_command_usage(user_id, user_name, command_name)

def fetch_command_info():
    command_lines = list(map(str.strip, open("command_output.txt", "r", encoding="UTF-8").readlines()))
    return random.choice(command_lines).replace("\\n", "\n")

def log_command_usage(user_id, user_name, command_name):
    date_time_string = str(datetime.datetime.now())
    info_string = date_time_string + " Command \"" + command_name + "\" used by " + user_id + " " + user_name
    print(info_string)
    log_file = open("log_file.txt", "a")
    log_file.write(info_string + "\n")

bot.run(os.getenv("TOKEN"))
