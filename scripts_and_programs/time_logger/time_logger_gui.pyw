import tkinter
import tkinter.ttk
import datetime
import time
import os
import threading
import configparser

start_time = datetime.datetime.now()
tag_type = ""
timer_enabled = False
button_rows = 0
button_columns = 0

config = configparser.ConfigParser()
config.read(os.path.dirname(__file__) + "/" + "config.ini", encoding = "utf-8")
save_path = config["config"]["log_file_path"].strip()
width_scale = int(config["config"]["width_scale"].strip())
height_scale = int(config["config"]["height_scale"].strip())
font_scale = int(config["config"]["font_scale"].strip())

def window_size_watchdog():
    global selection_label, time_label, start_root_size
    current_scale = 1
    while True:
        if (root.winfo_width() / start_root_size[0]) != current_scale:
            current_scale = root.winfo_width() / start_root_size[0]
            button_style = tkinter.ttk.Style()
            button_style.configure("TButton", font = ("TkDefaultFont", int(10 * font_scale * current_scale)))
            selection_label.config(font = ("TkDefaultFont", int(10 * font_scale * current_scale)))
            time_label.config(font = ("TkDefaultFont", int(20 * font_scale * current_scale)))

        time.sleep(0.1)

def generate_buttons(buttons):
    global button_rows, button_columns, frame
    button_style = tkinter.ttk.Style()
    button_style.configure("TButton", font=("TkDefaultFont", 10 * font_scale), justify = "center")
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

        button = button.replace("\\n", "\n")

        button_object = tkinter.ttk.Button(frame, text = button, style = "TButton", command = lambda button = button : reset(button)) #`button = button` is required to not lose reference on the button name
        button_object.grid(column = column, row = row, ipady = 25, pady = 5, sticky = "EWNS")

        button_objects.append(button_object)

        frame.columnconfigure(column, weight = 1)
        frame.rowconfigure(row, weight = 1)

        column += 1
        if column > button_columns:
            button_columns = column

    return button_objects

def edit_buttons(buttons, state):
    for button in buttons:
        button.config(state = state)

def start_timer():
    global timer_enabled, time_label
    timer_enabled = True
    while timer_enabled:
        time_label.config(text = str((datetime.datetime.now() - start_time) // 1000000 * 1000000))
        root.update()
        time.sleep(1)

def reset_timer():
    global timer_enabled, time_label
    timer_enabled = False
    time_label.config(text = "0:00:00")

def reset(new_tag_type):
    global start_time, tag_type, selection_label
    start_time = datetime.datetime.now()
    tag_type = new_tag_type

    selection_label.config(text = tag_type)

    edit_buttons(top_buttons, tkinter.DISABLED)
    edit_buttons(bottom_buttons, tkinter.ACTIVE)

    threading.Thread(target = start_timer).start()

def safe_csv_field(input_string):
    return ("\"" + str(input_string).replace("\"", "\"\"").replace("\n", " ").replace("\t", " ") + "\"")

def record():
    global start_time, tag_type, save_path, selection_label
    if not os.path.isabs(save_path):
        save_path = os.path.dirname(__file__) + "/" + save_path

    with open(save_path, "a", encoding = "utf-8") as logfile:
        logfile.write(str(datetime.datetime.now()))
        logfile.write(",")
        logfile.write(str(datetime.datetime.now() - start_time))
        logfile.write(",")
        logfile.write(safe_csv_field(tag_type))
        logfile.write("\n")

    selection_label.config(text = "")
    
    edit_buttons(bottom_buttons, tkinter.DISABLED)
    edit_buttons(top_buttons, tkinter.ACTIVE)

    reset_timer()

def discard():
    global selection_label
    selection_label.config(text = "")
    
    edit_buttons(bottom_buttons, tkinter.DISABLED)
    edit_buttons(top_buttons, tkinter.ACTIVE)

    reset_timer()

root = tkinter.Tk()
root.title("Time Logger")
root.columnconfigure(0, weight = 1)
root.rowconfigure(0, weight = 1)

frame = tkinter.ttk.Frame(root, padding = 10)
frame.grid(sticky = "EWNS")

top_buttons = generate_buttons(map(str.strip, config["config"]["top_buttons"].split(",")))

selection_label = tkinter.ttk.Label(frame, text = "")
selection_label.grid(column = 0, row = button_rows + 1, columnspan = button_columns)
selection_label.config(font = ("TkDefaultFont", 10 * font_scale))
frame.rowconfigure(button_rows + 1, weight = 1)

time_label = tkinter.ttk.Label(frame, text = "0:00:00")
time_label.grid(column = 0, row = button_rows + 2, columnspan = button_columns)
time_label.config(font = ("TkDefaultFont", 20 * font_scale))
frame.rowconfigure(button_rows + 2, weight = 1)

record_button = tkinter.ttk.Button(frame, text = config["config"]["record_button_name"].strip(), state = tkinter.DISABLED, command = record)
record_button.grid(column = 0, row = button_rows + 3, ipady = 25, pady = 5, sticky = "EWNS")
discard_button = tkinter.ttk.Button(frame, text = config["config"]["discard_button_name"].strip(), state = tkinter.DISABLED, command = discard)
discard_button.grid(column = button_columns - 1, row = button_rows + 3, ipady=25, pady = 5, sticky = "EWNS")
frame.rowconfigure(button_rows + 3, weight = 1)

bottom_buttons = [record_button, discard_button]

root.update()
start_root_size = (root.winfo_reqwidth(), root.winfo_reqheight())
root.geometry(str(start_root_size[0] * width_scale) + "x" + str(start_root_size[1] * height_scale))

threading.Thread(target = window_size_watchdog).start()

root.mainloop()
