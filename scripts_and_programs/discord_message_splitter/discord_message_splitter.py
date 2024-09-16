max_message_chars = 2000

file_lines = open(input("Message file: "), "r", encoding="UTF-8").readlines()

message_list = []

current_message = ""
for line in file_lines:
    if len(line) > max_message_chars:
        print("Error, found line too long")
        break
    if len(current_message) + len(line) <= max_message_chars:
        current_message += line
    else:
        message_list.append(current_message)
        current_message = ""


from tkinter import Tk
r = Tk()
r.withdraw()
print("Split " + str(len("".join(file_lines))) + " characters into " + str(len(message_list)) + " messages")
for i, message in enumerate(message_list):
    r.clipboard_clear()
    r.clipboard_append(message)
    r.update()
    input("Message part " + str(i + 1) + " copied to clipboard, press enter for next message part")
