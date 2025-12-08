# Free Badge Bot

A discord bot with slash commands that return a string of text.

## Usage

1. Set up your Discord bot token in one of two ways:

    - Set an env variable named `FREE_BADGE_BOT_DISCORD_TOKEN` to your token.

    - Create a file named `.env` in the directory you are running the free badge bot from containing `TOKEN=` followed by your token.

2. Create `free_badge_file.txt` and `fun_command_file.txt` in the directory you are running the bot.

3. Run the bot:

    ```
    cargo run --release
    ```

## Commands

- `/free badge`: Returns the contents of a random line in `free_badge_file.txt`.

- `/fun command`: Returns the contents of a random line in `fun_command_file.txt`.

- `/reload`: Reloads the contents of `free_badge_file.txt` and `fun_command_file.txt`.
