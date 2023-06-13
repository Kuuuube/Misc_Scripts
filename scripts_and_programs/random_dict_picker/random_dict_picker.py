import json
import random
import sys
import argparse
import os

def get_items_dict(input_dict, count):
    random_items = []
    for i in range(count):
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
    while True:
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

        check_count = input("\n")
        sys.stdout.write("\033[F\033[K\n")
        if check_count.replace("-c ", "").isdigit():
            items_count = int(check_count.replace("-c ", ""))
        elif check_count.lower().strip() == "--flip":
            json_dict = {v: k for k, v in json_dict.items()}
            key_delimiter, value_delimiter = value_delimiter, key_delimiter

def repeat_mode(json_list, key_delimiter, items_count):
    while True:
        random_items = get_items_list(json_list, items_count)

        base_string = ""
        for random_item in random_items:
            base_string += random_item
            print(random_item, end="")
            print(key_delimiter, end="")
        repeat_string = input("\n")

        write_string_diff(base_string, repeat_string)

        check_count = input()
        sys.stdout.write("\033[F\033[K\n")
        if check_count.replace("-c ", "").isdigit():
            items_count = int(check_count.replace("-c ", ""))



args_parser = argparse.ArgumentParser()
args_parser.add_argument("-f", metavar="FILE", required=True, help="json dict filepath to read")
args_parser.add_argument("-m", metavar="MODE", help="(flashcard|repeat)")
args_parser.add_argument("-c", metavar="INT", type=int, help="item count to display")
args_parser.add_argument("--flip", action="store_true", help="flip keys and values")
args_parser.add_argument("-k", metavar="STR", help="dict key padder")
args_parser.add_argument("-v", metavar="STR", help="dict value padder")
args = args_parser.parse_args()

try:
    json_dict = json.load(open(args.f, "r"))
except Exception as e:
    print(e)
    sys.exit(1)

key_delimiter = maybe(args.k, "")
value_delimiter = maybe(args.v, "")
items_count = maybe(args.c, 1)
mode = maybe(args.m, "flashcard")

if args.flip:
    json_dict = {v: k for k, v in json_dict.items()}
    key_delimiter, value_delimiter = value_delimiter, key_delimiter

try:
    os.system("cls" if os.name == "nt" else "clear")
    if mode == "flashcard":
        flashcard_mode(json_dict, key_delimiter, value_delimiter, items_count)
    elif mode == "repeat":
        repeat_mode(list(json_dict.keys()), key_delimiter, items_count)
except KeyboardInterrupt:
    sys.exit(0)
