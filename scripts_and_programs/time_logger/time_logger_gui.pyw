from tkinter import *
from tkinter import ttk
import datetime
import time
import os
import threading
import configparser

start_time = datetime.datetime.now()
immersion_type = ""
timer_enabled = False
config = configparser.ConfigParser()
config.read("config.ini", encoding="utf-8")
button_rows = 0
button_columns = 0

def generate_buttons(buttons):
    global button_rows, button_columns
    button_objects = []
    column = 0
    row = 0
    for button in buttons:
        if button == "\\n":
            row += 1
            column = 0
            button_rows += 1
            continue
        if button == "":
            column += 1
            continue
        button_object = ttk.Button(frm, text = button, command = lambda: reset(button))
        button_object.grid(column = column, row = row, ipady = 25, pady = 5)

        button_objects.append(button_object)
        column += 1
        if column > button_columns:
            button_columns = column

    return button_objects

def disable_buttons(buttons):
    for button in buttons:
        button.config(state = DISABLED)

def enable_buttons(buttons):
    for button in buttons:
        button.config(state = ACTIVE)

def set_selection_label(label_text):
    selection_label.config(text = label_text)

def start_timer():
    global timer_enabled
    timer_enabled = True
    while timer_enabled:
        time_label.config(text = str((datetime.datetime.now() - start_time) // 1000000 * 1000000))
        root.update()
        time.sleep(1)

def reset_timer():
    global timer_enabled
    timer_enabled = False
    time_label.config(text = "0:00:00")

def reset(new_immersion_type):
    global start_time, immersion_type
    start_time = datetime.datetime.now()
    immersion_type = new_immersion_type

    set_selection_label(immersion_type)

    disable_buttons(top_buttons)
    enable_buttons(bottom_buttons)

    threading.Thread(target=start_timer).start()

def record():
    global start_time, immersion_type
    save_path = config["config"]["log_file_path"].strip()
    if not os.path.isabs(save_path):
        save_path = os.path.dirname(__file__) + "\\" + save_path

    with open(save_path, "a", encoding = "utf-8") as logfile:
        logfile.write(str(datetime.datetime.now()))
        logfile.write("\t")
        logfile.write(str(datetime.datetime.now() - start_time))
        logfile.write("\t")
        logfile.write(immersion_type)
        logfile.write("\n")

    set_selection_label("")
    
    disable_buttons(bottom_buttons)
    enable_buttons(top_buttons)

    reset_timer()

def discard():
    set_selection_label("")
    
    disable_buttons(bottom_buttons)
    enable_buttons(top_buttons)

    reset_timer()

root = Tk()
root.title("Time Logger")
frm = ttk.Frame(root, padding=10)
frm.grid()

top_buttons = generate_buttons(map(str.strip, config["config"]["top_buttons"].split(",")))

selection_label = ttk.Label(frm, text = "")
selection_label.grid(column = 0, row = button_rows + 1, columnspan = button_columns)

time_label = ttk.Label(frm, text = "0:00:00")
time_label.grid(column = 0, row = button_rows + 2, columnspan = button_columns)
time_label.config(font=("TkDefaultFont", 20))

record_button = ttk.Button(frm, text = config["config"]["record_button_name"].strip(), state = DISABLED, command = record)
record_button.grid(column = 0, row = button_rows + 3, sticky = "", ipady=25, pady = 5)
discard_button = ttk.Button(frm, text = config["config"]["discard_button_name"].strip(), state = DISABLED, command = discard)
discard_button.grid(column = button_columns - 1, row = button_rows + 3, sticky = "", ipady=25, pady = 5)

bottom_buttons = [record_button, discard_button]

root.mainloop()
