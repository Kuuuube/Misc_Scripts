import json
import random
import sys
import argparse
import os
import collections
import unicodedata
import contextlib

settings_tuple = collections.namedtuple("settings", "init json_path json_dict key_delimiter value_delimiter items_count mode clear toprowpad botrowpad")

def parse_args(args_list, settings = settings_tuple(False, None, None, "", "", 1, "flashcard", False, 0, 0)):
    args_parser = argparse.ArgumentParser(add_help=False)
    if not settings.init:
        args_parser.add_argument("-h", "--help", action="help", help="show this help message and exit.")
        args_parser.add_argument("-f", metavar="FILE", required=True, help="json dict filepath to read")
    else:
        args_parser.add_argument("-h", "--help", action="store_true", help="show this help message and exit.")
        args_parser.add_argument("-f", metavar="FILE", help="json dict filepath to read")
    args_parser.add_argument("-m", metavar="MODE", help="(flashcard|repeat)")
    args_parser.add_argument("-c", metavar="INT", type=int, help="item count to display")
    args_parser.add_argument("--flip", action="store_true", help="flip keys and values")
    args_parser.add_argument("-k", metavar="STR", help="dict key padder")
    args_parser.add_argument("-v", metavar="STR", help="dict value padder")
    args_parser.add_argument("-r", action="store_true", help="reload the current json file")
    args_parser.add_argument("--clear", action="store_true", help="toggle clearing after each prompt")
    args_parser.add_argument("--toprowpad", metavar="INT", type=int, help="row padding in newlines above each prompt")
    args_parser.add_argument("--botrowpad", metavar="INT", type=int, help="row padding in newlines below each prompt")

    try:
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

    json_path = settings.json_path
    json_dict = settings.json_dict

    if args.f:
        try:
            json_path = args.f
            json_dict = json.load(open(json_path, "r", encoding="utf-8"))
        except Exception as e:
            if not settings.init:
                print(e)
                sys.exit(1)
            input("Failed to load file, press enter to continue...")
            remove_wrapped_string("Failed to load file, press enter to continue...")

    if args.r:
        try:
            json_dict = json.load(open(json_path, "r", encoding="utf-8"))
        except Exception as e:
            if not settings.init:
                print(e)
                sys.exit(1)
            input("Failed to reload file, press enter to continue...")
            remove_wrapped_string("Failed to reload file, press enter to continue...")

    mode = maybe_enum(args.m, ["flashcard", "repeat"], settings.mode)

    items_count = maybe(args.c, settings.items_count)

    if args.flip:
        json_dict = {v: k for k, v in settings.json_dict.items()}
        key_delimiter, value_delimiter = settings.value_delimiter, settings.key_delimiter

    key_delimiter = maybe(args.k, settings.key_delimiter)

    value_delimiter = maybe(args.v, settings.value_delimiter)

    clear = args.clear != settings.clear

    toprowpad = maybe(args.toprowpad, settings.toprowpad)

    botrowpad = maybe(args.botrowpad, settings.botrowpad)

    return settings_tuple(True, json_path, json_dict, key_delimiter, value_delimiter, items_count, mode, clear, toprowpad, botrowpad)

def request_args(settings):
    check_for_args = input(":")
    remove_wrapped_string(":" + check_for_args)
    sys.stdout.write("\n\n")
    if check_for_args != "":
        new_settings = parse_args(check_for_args.split(" "), settings)
        return new_settings

def get_items_dict(input_dict, count):
    random_items = []
    for _ in range(count):
        random_items.append(random.choice(list(input_dict.items())))
    return random_items

def get_items_list(input_list, count):
    random_items = []
    for _ in range(count):
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

def add_top_padding(padding):
    for _ in range(padding):
        sys.stdout.write("\n")

def add_bottom_padding(padding):
    for _ in range(padding):
        sys.stdout.write("\n")
    for _ in range(padding):
        sys.stdout.write("\033[A")
    sys.stdout.write("\033[F")

def remove_wrapped_string(string):
    columns, _ = os.get_terminal_size()

    string_width = 1
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

    if string_width < columns:
        string_width = columns
    for _ in range(-(-string_width // columns)):
        sys.stdout.write("\033[F\033[K")

def write_string_diff(base_string, repeat_string):
    remove_wrapped_string(repeat_string)
    for base, repeat in zip(base_string, repeat_string):
        #rgb terminal text \033[38;2;<r>;<g>;<b>m
        if repeat == base:
            sys.stdout.write("\033[38;2;0;255;0m") #green
        else:
            sys.stdout.write("\033[38;2;255;0;0m") #red
        sys.stdout.write(repeat)
        sys.stdout.write("\033[39m\033[49m") #reset color
    sys.stdout.write("\n")

def flashcard_mode(json_dict, key_delimiter, value_delimiter, items_count):
    random_items = get_items_dict(json_dict, items_count)

    base_string = ""
    for random_item in random_items:
        base_string += random_item[1] + value_delimiter
        print(random_item[0] + key_delimiter, end="")

    repeat_string = input("\n")
    if repeat_string == "":
        sys.stdout.write("\033[F")
    else:
        write_string_diff(base_string, repeat_string)

    for random_item in random_items:
        print(random_item[1] + value_delimiter, end="")
    sys.stdout.write("\n")

def repeat_mode(json_dict, key_delimiter, items_count):
    json_list = list(json_dict.keys())
    random_items = get_items_list(json_list, items_count)

    base_string = ""
    for random_item in random_items:
        base_string += random_item
        print(random_item, end="")
        print(key_delimiter, end="")
    repeat_string = input("\n")

    write_string_diff(base_string, repeat_string)



settings = parse_args(sys.argv[1:])

try:
    os.system("cls" if os.name == "nt" else "clear")
    add_top_padding(settings.toprowpad)
    while True:
        add_bottom_padding(settings.botrowpad)

        if settings.mode == "flashcard":
            flashcard_mode(settings.json_dict, settings.key_delimiter, settings.value_delimiter, settings.items_count)
        elif settings.mode == "repeat":
            repeat_mode(settings.json_dict, settings.key_delimiter, settings.items_count)
        else:
            print("Invalid mode selected")
            sys.exit(1)

        settings = maybe(request_args(settings), settings)

        if settings.clear:
            os.system("cls" if os.name == "nt" else "clear")

        add_top_padding(settings.toprowpad)
except KeyboardInterrupt:
    sys.exit(0)
