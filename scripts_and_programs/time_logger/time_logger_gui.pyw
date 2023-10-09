from tkinter import *
from tkinter import ttk
import datetime
import time
import os
import threading

start_time = datetime.datetime.now()
immersion_type = ""
timer_enabled = False

def disable_buttons(buttons):
    for button in buttons:
        button.config(state = DISABLED)

def enable_buttons(buttons):
    for button in buttons:
        button.config(state = ACTIVE)

def set_selection_label(label_text):
    selection_label.config(text = label_text)

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
    with open(os.path.dirname(__file__) + "\\" + "log.csv", "a") as logfile:
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

root = Tk()
root.title("Time Logger")
frm = ttk.Frame(root, padding=10)
frm.grid()

anime_button = ttk.Button(frm, text = "Anime", command = lambda: reset("Anime"))
anime_button.grid(column = 0, row = 0, ipady = 25, pady = 5)
manga_button = ttk.Button(frm, text = "Manga", command = lambda: reset("Manga"))
manga_button.grid(column = 1, row = 0, ipady = 25, pady = 5)
ln_button = ttk.Button(frm, text = "LN", command = lambda: reset("LN"))
ln_button.grid(column = 2, row = 0, ipady = 25, pady = 5)
vn_button = ttk.Button(frm, text = "VN", command = lambda: reset("VN"))
vn_button.grid(column = 3, row = 0, ipady = 25, pady = 5)
youtube_button = ttk.Button(frm, text = "Youtube", command = lambda: reset("Youtube"))
youtube_button.grid(column = 4, row = 0, ipady = 25, pady = 5)

selection_label = ttk.Label(frm, text = "")
selection_label.grid(column = 0, row = 1, columnspan = 5)

time_label = ttk.Label(frm, text = "0:00:00")
time_label.grid(column = 0, row = 2, columnspan = 5)
time_label.config(font=("TkDefaultFont", 20))

record_button = ttk.Button(frm, text = "Record", state = DISABLED, command = record)
record_button.grid(column = 0, row = 3, sticky = "", ipady=25, pady = 5)
discard_button = ttk.Button(frm, text = "Discard", state = DISABLED, command = discard)
discard_button.grid(column = 4, row = 3, sticky = "", ipady=25, pady = 5)

top_buttons = [anime_button, manga_button, ln_button, vn_button, youtube_button]
bottom_buttons = [record_button, discard_button]

root.mainloop()
