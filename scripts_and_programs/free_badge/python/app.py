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
    registered_commands = []
    for application_command in bot.get_all_application_commands():
        registered_commands.append(application_command.name)
    print('Logged in as ' + bot.user.name + ' (' + str(bot.user.id) + ') Registered commands: ' + ", ".join(registered_commands))

@bot.slash_command(name = "free_badge", description="Free Badge")
async def free_badge(interaction: nextcord.Interaction):
    user_id = str(interaction.user.id)
    user_name = str(interaction.user.name) + "#" + str(interaction.user.discriminator)
    command_name = str(interaction.application_command.name)
    command_info = fetch_command_info("command_output.txt")
    await interaction.send(command_info, ephemeral=True)
    log_command_usage(user_id, user_name, command_name, command_info)

@bot.slash_command(name = "fun_command", description="Fun Command")
async def fun_command(interaction: nextcord.Interaction):
    user_id = str(interaction.user.id)
    user_name = str(interaction.user.name) + "#" + str(interaction.user.discriminator)
    command_name = str(interaction.application_command.name)
    command_info = fetch_command_info("command_output2.txt")
    await interaction.send(command_info, ephemeral=True)
    log_command_usage(user_id, user_name, command_name, command_info)

def fetch_command_info(file):
    command_lines = list(map(str.strip, open(file, "r", encoding="UTF-8").readlines()))
    return random.choice(command_lines).replace("\\n", "\n")

def log_command_usage(user_id, user_name, command_name, command_info):
    date_time_string = str(datetime.datetime.now())
    info_string = date_time_string + " Command \"" + command_name + "\" used by " + user_id + " " + user_name + " Command output: " + command_info
    print(info_string)
    log_file = open("log_file.txt", "a")
    log_file.write(info_string + "\n")

bot.run(os.getenv("TOKEN"))
