# Random Dict Picker

Command line study tool. Supports flashcards and repetition practice.

# Usage

- Run `random_dict_picker.py` with args

## Args

```
usage: random_dict_picker.py [-h] -f FILE [-m MODE] [-c INT] [--flip] [-k STR] [-v STR] [-r] [--time] [--clear] [--toprowpad FLOAT] [--botrowpad FLOAT]
```

### Mandatory:

- `-f`: Json dict filepath to read. An example file, `example_dict.json`, has been included.

### Optional:

- `-m`: `flashcard` or `repeat`. Default: `flashcard`.

- `-c`: Item count to display at once. Default: `1`. Use `0` to attempt to fill one full line in the terminal.

- `--flip`: Flip keys and values.

- `-k`: Dict key padder. This string will be printed between each key.

- `-v`: Dict value padder. This string will be printed between each value.

- `-r`: Reload the current json dict.

- `--time`: Show time taken after each prompt.

- `--clear`: Toggle clearing after each prompt.

- `--toprowpad`: Row padding in newlines above each prompt. Values <1 use a percent of the terminal size.

- `--botrowpad`: Row padding in newlines below each prompt. Values <1 use a percent of the terminal size.

Example args (uses `example_dict.json`, displays 10 dict items at once, uses repeat mode, and adds a space for padding between keys):

```
random_dict_picker.py -f example_dict.json -c 10 -m repeat -k " "
```

## Modes

### Flashcard Mode:

- Flashcard mode displays a key (or value if `--flip` is invoked). 

- Optionally, you may input the value associated with the key. Your input will be checked against the value and display green or red for correct or incorrect.

- Pressing enter will reveal the value.

- Optionally, at the `:` you may input any args to update the program state.

- Pressing enter again will show the next key.

### Repeat Mode:

- Repeat mode displays a key (or value if `--flip` is invoked). 

- You should input the text that is displayed.

- Pressing enter will check they key against your input and display green or red for correct or incorrect.

- Optionally, at the `:` you may input any args to update the program state.

- Pressing enter again will show the next key.

# Dependencies

Python 3: [Download link](https://www.python.org/downloads/)

# Notes

- In the json file, the key is first and the value is second. For example, `"key": "value",`.

- If you have trouble displaying anything, you may need a better terminal emulator. 

    I recommend alacritty, you can find my terminal emulator and shell settings [here](https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/terminal_and_shell_config/).