import tkinter
import tkinter.ttk
import datetime
import time
import os
import threading
import configparser
import codecs
import re
import math

start_time = datetime.datetime.now()
tag_type = ""
timer_enabled = False
button_rows = 0
button_columns = 0

def maybe_read_config(maybe, section, name = None):
    try:
        config = configparser.ConfigParser()
        config.read(os.path.dirname(__file__) + "/" + "config.ini", encoding="utf-8")

        if name == None:
            return config[section]

        return config[section][name]

    except Exception:
        return maybe

save_path = maybe_read_config("log.csv", "config", "log_file_path").strip()
width_scale = int(maybe_read_config("1", "config", "width_scale").strip())
height_scale = int(maybe_read_config("1", "config", "height_scale").strip())
font_scale = int(maybe_read_config("1", "config", "font_scale").strip())
current_scale = 1
show_notes_box = maybe_read_config("false", "config", "show_notes_box").strip() == "true"

def dynamic_font_scaler():
    global selection_label, time_label, start_root_size, current_scale
    if (root.winfo_width() / start_root_size[0]) != current_scale:
        current_scale = root.winfo_width() / start_root_size[0]
        button_style = tkinter.ttk.Style()
        button_style.configure("TButton", font = ("TkDefaultFont", int(10 * font_scale * current_scale)))
        selection_label.config(font = ("TkDefaultFont", int(10 * font_scale * current_scale)))
        time_label.config(font = ("TkDefaultFont", int(20 * font_scale * current_scale)))
        entry_box.config(font = ("TkDefaultFont", int(20 * font_scale * current_scale)))
        if show_notes_box:
            notes_box.config(font = ("TkDefaultFont", int(20 * font_scale * current_scale)))

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

        button = codecs.decode(button, "unicode_escape")

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

def reset_entry_box():
    global entry_box
    entry_box.delete(0,tkinter.END)
    entry_box.insert(0, "0:00:00")

def reset_notes_box():
    if show_notes_box:
        global notes_box
        notes_box.delete(0,tkinter.END)

def reset(new_tag_type):
    global start_time, tag_type, selection_label
    start_time = datetime.datetime.now()
    tag_type = new_tag_type

    selection_label.config(text = tag_type)

    edit_buttons(top_buttons, tkinter.DISABLED)
    edit_buttons(bottom_buttons, tkinter.ACTIVE)
    entry_box.config(state = tkinter.ACTIVE)
    if show_notes_box:
        notes_box.config(state = tkinter.ACTIVE)
    reset_entry_box()
    reset_notes_box()

    threading.Thread(target = start_timer).start()

def safe_csv_field(input_string):
    return ("\"" + str(input_string).replace("\"", "\"\"").replace("\n", " ").replace("\t", " ").replace("\r", " ") + "\"")

def record():
    global start_time, tag_type, save_path, selection_label
    if not os.path.isabs(save_path):
        save_path = os.path.dirname(__file__) + "/" + save_path

    with open(save_path, "a", encoding = "utf-8") as logfile:
        logfile.write(str(datetime.datetime.now()))
        logfile.write(",")
        logged_time = ""
        if re.search("\d+:\d{1,2}:\d{1,2}(\.\d+|)", entry_box.get()) != None and entry_box.get() != "0:00:00":
            logged_time = str(re.sub("\..*", "", entry_box.get())) + ".000000"
        else:
            logged_time = str(datetime.datetime.now() - start_time)
        logfile.write(logged_time)
        logfile.write(",")
        logfile.write(safe_csv_field(tag_type))
        if show_notes_box:
            logfile.write(",")
            logfile.write(safe_csv_field(notes_box.get()))
        logfile.write("\n")

    selection_label.config(text = tag_type + ": " + logged_time[:-7])
    reset_entry_box()
    reset_notes_box()

    edit_buttons(bottom_buttons, tkinter.DISABLED)
    edit_buttons(top_buttons, tkinter.ACTIVE)
    entry_box.config(state = tkinter.DISABLED)
    if show_notes_box:
        notes_box.config(state = tkinter.DISABLED)

    reset_timer()

def discard():
    global selection_label
    selection_label.config(text = "")
    reset_entry_box()
    reset_notes_box()
    
    edit_buttons(bottom_buttons, tkinter.DISABLED)
    edit_buttons(top_buttons, tkinter.ACTIVE)
    entry_box.config(state = tkinter.DISABLED)
    if show_notes_box:
        notes_box.config(state = tkinter.DISABLED)

    reset_timer()

def time_logger_graph():
    graph_type = maybe_read_config("plot", "config", "graph_type").strip()
    graph_x_grid = True if maybe_read_config("true", "config", "graph_x_grid").strip() == "true" else False
    graph_y_grid = True if maybe_read_config("true", "config", "graph_y_grid").strip() == "true" else False
    graph_stacked = True if maybe_read_config("true", "config", "graph_stacked").strip() == "true" else False
    graph_legend = True if maybe_read_config("true", "config", "graph_legend").strip() == "true" else False
    csv_has_header = True if maybe_read_config("true", "config", "csv_has_header").strip() == "true" else False
    graph_day_offset = float(maybe_read_config(0, "config", "graph_day_offset").strip())
    import graph
    graph.show_graph(graph_type, graph_x_grid, graph_y_grid, graph_stacked, graph_legend, csv_has_header, graph_day_offset)

root = tkinter.Tk()
root.title("Time Logger")
root.columnconfigure(0, weight = 1)
root.rowconfigure(0, weight = 1)

frame = tkinter.ttk.Frame(root, padding = 10)
frame.grid(sticky = "EWNS")

top_buttons_list = list(map(str.strip, maybe_read_config("", "config", "top_buttons").split(",")))
if len(top_buttons_list) <= 1: #prevent broken configs making UI unrunnable
    top_buttons_list.append(" ")
top_buttons = generate_buttons(top_buttons_list)

selection_label = tkinter.ttk.Label(frame, text = "")
selection_label.grid(column = 0, row = button_rows + 1, columnspan = button_columns)
selection_label.config(font = ("TkDefaultFont", 10 * font_scale))
frame.rowconfigure(button_rows + 1, weight = 1)

time_label = tkinter.ttk.Label(frame, text = "0:00:00")
time_label.grid(column = 0, row = button_rows + 2, columnspan = button_columns // 2)
time_label.config(font = ("TkDefaultFont", 20 * font_scale))

entry_box = tkinter.ttk.Entry(frame, width = 1, justify="center")
entry_box.grid(column = math.ceil(button_columns / 2), row = button_rows + 2, columnspan = button_columns // 2, sticky = "EWNS")
reset_entry_box()
entry_box.config(font = ("TkDefaultFont", 20 * font_scale), state = tkinter.DISABLED)
frame.rowconfigure(button_rows + 2, weight = 1)

if show_notes_box:
    button_rows += 1
    notes_box = tkinter.ttk.Entry(frame, width = 1, justify="left")
    notes_box.grid(column = 0, row = button_rows + 2, columnspan = button_columns, pady = 5, sticky = "EWNS")
    notes_box.config(font = ("TkDefaultFont", 20 * font_scale), state = tkinter.DISABLED)
    frame.rowconfigure(button_rows + 2, weight = 1)

record_button = tkinter.ttk.Button(frame, text = maybe_read_config("Record", "config", "record_button_name").strip(), state = tkinter.DISABLED, command = record)
record_button.grid(column = 0, row = button_rows + 3, ipady = 25, pady = 5, sticky = "EWNS")

graph_button = tkinter.ttk.Button(frame, text = maybe_read_config("Graph", "config", "graph_button_name").strip(), command = time_logger_graph)
graph_button.grid(column = button_columns // 2, row = button_rows + 3, ipady = 25, pady = 5, sticky = "EWNS")

discard_button = tkinter.ttk.Button(frame, text = maybe_read_config("Discard", "config", "discard_button_name").strip(), state = tkinter.DISABLED, command = discard)
discard_button.grid(column = button_columns - 1, row = button_rows + 3, ipady=25, pady = 5, sticky = "EWNS")
frame.rowconfigure(button_rows + 3, weight = 1)

bottom_buttons = [record_button, discard_button]

root.update()
start_root_size = (root.winfo_reqwidth(), root.winfo_reqheight())
root.geometry(str(start_root_size[0] * width_scale) + "x" + str(start_root_size[1] * height_scale))

root.bind("<Configure>",  lambda event : dynamic_font_scaler())

root.mainloop()
