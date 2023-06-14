import json
import random
import sys
import argparse
import os

json_path = None
json_dict = None
key_delimiter = ""
value_delimiter = ""
items_count = 1
mode = "flashcard"

def parse_args(args_list, json_path, json_dict, key_delimiter, value_delimiter, items_count, mode):
    args_parser = argparse.ArgumentParser()
    if json_path == None:
        args_parser.add_argument("-f", metavar="FILE", required=True, help="json dict filepath to read")
    else:
        args_parser.add_argument("-f", metavar="FILE", help="json dict filepath to read")
    args_parser.add_argument("-m", metavar="MODE", help="(flashcard|repeat)")
    args_parser.add_argument("-c", metavar="INT", type=int, help="item count to display")
    args_parser.add_argument("--flip", action="store_true", help="flip keys and values")
    args_parser.add_argument("-k", metavar="STR", help="dict key padder")
    args_parser.add_argument("-v", metavar="STR", help="dict value padder")
    args_parser.add_argument("-r", action="store_true", help="reload the current json file")
    if json_path == None:
        args = args_parser.parse_args(args=args_list)
    else:
        args, _ = args_parser.parse_known_args(args=args_list)

    if json_path == None:
        try:
            json_path = args.f
            json_dict = json.load(open(json_path, "r", encoding="utf-8"))
        except Exception as e:
            print(e)
            sys.exit(1)
    elif args.r:
        try:
            json_dict = json.load(open(json_path, "r", encoding="utf-8"))
        except Exception as e:
            print(e)
            sys.exit(1)

    key_delimiter = maybe(args.k, key_delimiter)

    value_delimiter = maybe(args.v, value_delimiter)

    items_count = maybe(args.c, items_count)

    mode = maybe_enum(args.m, ["flashcard", "repeat"], mode)

    if args.flip:
        json_dict = {v: k for k, v in json_dict.items()}
        key_delimiter, value_delimiter = value_delimiter, key_delimiter

    return json_path, json_dict, key_delimiter, value_delimiter, items_count, mode

def get_items_dict(input_dict, count):
    random_items = []
    for _ in range(count):
        random_items.append(random.choice(list(input_dict.items())))
    return random_items

def get_items_list(input_list, count):
    random_items = []
    for i in range(count):
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

def write_string_diff(base_string, repeat_string):
    sys.stdout.write("\033[F\033[K")
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



json_path, json_dict, key_delimiter, value_delimiter, items_count, mode = parse_args(sys.argv[1:], json_path, json_dict, key_delimiter, value_delimiter, items_count, mode)

try:
    os.system("cls" if os.name == "nt" else "clear")
    while True:
        if mode == "flashcard":
            flashcard_mode(json_dict, key_delimiter, value_delimiter, items_count)
        elif mode == "repeat":
            repeat_mode(json_dict, key_delimiter, items_count)
        else:
            print("Invalid mode selected")
            sys.exit(1)

        check_for_args = input()
        sys.stdout.write("\033[F\033[K\n")
        if check_for_args != "":
            json_path, json_dict, key_delimiter, value_delimiter, items_count, mode = parse_args(check_for_args.split(" "), json_path, json_dict, key_delimiter, value_delimiter, items_count, mode)
except KeyboardInterrupt:
    sys.exit(0)
