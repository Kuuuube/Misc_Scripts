import json
import random
import sys
import argparse
import os
import collections
import unicodedata
import contextlib
import timeit

settings_tuple = collections.namedtuple("settings", "init json_paths json_dicts key_delimiter value_delimiter items_count mode time clear toprowpad botrowpad")

def parse_args(args_list, settings = settings_tuple(False, [], [], "", "", 1, "flashcard", False, False, 0, 0)):
    args_parser = argparse.ArgumentParser(add_help=False, allow_abbrev=False)
    if not settings.init:
        args_parser.add_argument("-h", "--help", action="help", help="Show this help message and exit.")
        args_parser.add_argument("-f", metavar="FILE", required=True, action="append", help="Json dict filepath to read.")
    else:
        args_parser.add_argument("-h", "--help", action="store_true", help="Show this help message and exit.")
        args_parser.add_argument("-f", metavar="FILE", action="append", help="Json dict filepath to read.")
    args_parser.add_argument("-m", metavar="MODE", help="(flashcard|repeat)")
    args_parser.add_argument("-c", metavar="FLOAT", type=float, help="Item count to display. 0 will attempt to fill one full line in the terminal. Values <1 will attempt to fill a percent of the terminal size.")
    args_parser.add_argument("--flip", action="store_true", help="Flip dict keys and values.")
    args_parser.add_argument("-k", metavar="STR", help="Dict key padder. This string will be printed between each key.")
    args_parser.add_argument("-v", metavar="STR", help="Dict value padder. This string will be printed between each value.")
    args_parser.add_argument("-r", action="store_true", help="Reload the current json file")
    args_parser.add_argument("--time", action="store_true", help="Show time taken after each prompt")
    args_parser.add_argument("--clear", action="store_true", help="Toggle clearing after each prompt")
    args_parser.add_argument("--toprowpad", metavar="INT", type=float, help="Row padding in newlines above each prompt. Values <1 use a percent of the terminal size.")
    args_parser.add_argument("--botrowpad", metavar="INT", type=float, help="Row padding in newlines below each prompt. Values <1 use a percent of the terminal size.")

    try:
        if not settings.init:
            args = args_parser.parse_args(args=args_list)
        else:
            with contextlib.redirect_stderr(open(os.devnull, 'w')): #stop argparse from overreaching and printing its own errors
                args = args_parser.parse_args(args=args_list)
    except SystemExit:
        if not settings.init:
            sys.exit(0)

        input("Invalid argument, press enter to continue...")
        remove_wrapped_string("Invalid argument, press enter to continue...")
        return settings

    if settings.init and args.help:
        usage = args_parser.format_usage()
        sys.stdout.write(usage)
        new_settings = request_args(settings) #causes recursion
        remove_wrapped_string(usage)
        return new_settings

    json_paths = settings.json_paths
    json_dicts = settings.json_dicts

    if args.f:
        try:
            new_json_paths = args.f
            new_json_dicts = []
            for json_path in new_json_paths:
                new_json_dicts.append(json.load(open(json_path, "r", encoding="utf-8")))

            json_paths = new_json_paths
            json_dicts = new_json_dicts
        except Exception as e:
            if not settings.init:
                print(e)
                sys.exit(1)
            input("Failed to load file, press enter to continue...")
            remove_wrapped_string("Failed to load file, press enter to continue...")

    if args.r:
        try:
            new_json_dicts = []
            for json_path in json_paths:
                new_json_dicts.append(json.load(open(json_path, "r", encoding="utf-8")))

            json_dicts = new_json_dicts
        except Exception as e:
            if not settings.init:
                print(e)
                sys.exit(1)
            input("Failed to reload file, press enter to continue...")
            remove_wrapped_string("Failed to reload file, press enter to continue...")

    mode = maybe_enum(args.m, ["flashcard", "repeat"], settings.mode)

    items_count = maybe(args.c, settings.items_count)

    if args.flip:
        new_json_dicts = []
        for json_dict in settings.json_dicts:
            new_json_dicts.append({v: k for k, v in json_dict.items()})

        json_dicts = new_json_dicts
        key_delimiter, value_delimiter = settings.value_delimiter, settings.key_delimiter

    key_delimiter = maybe(args.k, settings.key_delimiter)

    value_delimiter = maybe(args.v, settings.value_delimiter)

    time = args.time != settings.time

    clear = args.clear != settings.clear

    toprowpad = maybe(args.toprowpad, settings.toprowpad)

    botrowpad = maybe(args.botrowpad, settings.botrowpad)

    return settings_tuple(True, json_paths, json_dicts, key_delimiter, value_delimiter, items_count, mode, time, clear, toprowpad, botrowpad)

def request_args(settings):
    check_for_args = input(":")
    remove_wrapped_string(":" + check_for_args)
    sys.stdout.write("\n\n")
    if check_for_args != "":
        new_settings = parse_args(check_for_args.split(" "), settings)
        return new_settings

def get_items_dict(input_dict, items_count):
    random_items = []
    if items_count == 0:
        while str_width("".join(k[0] for k in random_items)) <= os.get_terminal_size().columns:
            random_items.append(random.choice(list(input_dict.items())))
        random_items.pop() #list will always end up larger than terminal by one element
    elif items_count < 1:
        while str_width("".join(k[0] for k in random_items)) <= os.get_terminal_size().columns * items_count:
            random_items.append(random.choice(list(input_dict.items())))
        random_items.pop() #list will always end up larger than terminal percent by one element
    else:
        for _ in range(int(items_count)):
            random_items.append(random.choice(list(input_dict.items())))
    return random_items

def get_items_list(input_list, items_count):
    random_items = []
    if items_count == 0:
        while str_width("".join(random_items)) <= os.get_terminal_size().columns:
            random_items.append(random.choice(input_list))
        random_items.pop() #list will always end up larger than terminal by one element
    elif items_count < 1:
        while str_width("".join(random_items)) <= os.get_terminal_size().columns * items_count:
            random_items.append(random.choice(input_list))
        random_items.pop() #list will always end up larger than terminal percent by one element
    else:
        for _ in range(int(items_count)):
            random_items.append(random.choice(input_list))
    return random_items

def maybe(value, default):
    if value == None:
        return default
    else:
        return value

def maybe_enum(value, enum, default):
    if value not in enum:
        return default
    else:
        return value

def maybe_index(value, index, default):
    if index >= 0 and index < len(value):
        return value[index]
    else:
        return default

def add_top_padding(padding):
    if padding < 1:
        padding = int(os.get_terminal_size().lines * padding)
    else:
        padding = int(padding)
    for _ in range(padding):
        sys.stdout.write("\n")

def add_bottom_padding(padding):
    if padding < 1:
        padding = int(os.get_terminal_size().lines * padding)
    else:
        padding = int(padding)
    for _ in range(padding):
        sys.stdout.write("\n")
    for _ in range(padding):
        sys.stdout.write("\033[A")
    sys.stdout.write("\033[F")

def str_width(string):
    columns = os.get_terminal_size().columns
    string_width = 0
    for char in string:
        if char == "\n":
            string_width += columns
            continue

        char_width = unicodedata.east_asian_width(char)
        match char_width:
            case "A": string_width += 1 #Ambiguous, the terminal should renders these as narrow/halfwidth in almost all cases
            case "F": string_width += 2 #Fullwidth
            case "H": string_width += 1 #Halfwidth
            case "N": string_width += 1 #Neutral, the same as narrow for a terminal
            case "Na": string_width += 1 #Narrow
            case "W": string_width += 2 #Wide
    return string_width

def remove_wrapped_string(string):
    columns = os.get_terminal_size().columns

    string_width = str_width(string) + 1

    if string_width < columns:
        string_width = columns
    for _ in range(-(-string_width // columns)):
        sys.stdout.write("\033[F\033[K")

def write_string_diff(base_string, repeat_string):
    remove_wrapped_string(repeat_string)
    i = 0
    while i < len(repeat_string):
        base = maybe_index(base_string, i, "")
        repeat = maybe_index(repeat_string, i, "")
        #rgb terminal text \033[38;2;<r>;<g>;<b>m
        if repeat == base:
            sys.stdout.write("\033[38;2;0;255;0m") #green
        else:
            sys.stdout.write("\033[38;2;255;0;0m") #red
        sys.stdout.write(repeat)
        sys.stdout.write("\033[39m\033[49m") #reset color

        i += 1
    sys.stdout.write("\n")

def flashcard_mode(json_dict, key_delimiter, value_delimiter, items_count):
    random_items = get_items_dict(json_dict, items_count)

    base_string_key = ""
    base_string_val = ""
    for random_item in random_items:
        base_string_key += random_item[0] + key_delimiter
        base_string_val += random_item[1] + value_delimiter
    sys.stdout.write(base_string_key)

    repeat_string = input("\n")
    if repeat_string == "":
        sys.stdout.write("\033[F")
    else:
        write_string_diff(base_string_val, repeat_string)

    sys.stdout.write(base_string_val + "\n")

def repeat_mode(json_dict, key_delimiter, items_count):
    json_list = list(json_dict.keys())
    random_items = get_items_list(json_list, items_count)

    base_string = ""
    for random_item in random_items:
        base_string += random_item + key_delimiter

    sys.stdout.write(base_string)
    repeat_string = input("\n")

    write_string_diff(base_string, repeat_string)



settings = parse_args(sys.argv[1:])

try:
    os.system("cls" if os.name == "nt" else "clear")
    add_top_padding(settings.toprowpad)
    while True:
        add_bottom_padding(settings.botrowpad)

        start_time = timeit.default_timer()
        if settings.mode == "flashcard":
            flashcard_mode(random.choice(settings.json_dicts), settings.key_delimiter, settings.value_delimiter, settings.items_count)
        elif settings.mode == "repeat":
            repeat_mode(random.choice(settings.json_dicts), settings.key_delimiter, settings.items_count)
        else:
            print("Invalid mode selected")
            sys.exit(1)

        if settings.time:
            sys.stdout.write(str(round(timeit.default_timer() - start_time, 2)) + "s\n")

        settings = maybe(request_args(settings), settings)

        if settings.clear:
            os.system("cls" if os.name == "nt" else "clear")

        add_top_padding(settings.toprowpad)
except KeyboardInterrupt:
    sys.exit(0)
