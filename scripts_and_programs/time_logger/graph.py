import matplotlib.pyplot
import matplotlib.dates
import numpy
import datetime

def zero_filler(input_list, target_len):
    while len(input_list) < target_len:
        input_list.append(datetime.datetime(1900, 1, 1))
    return input_list

def setup_graph(graph_type, x_list, y_list, stacked, key, bottom_limit, top_limit, bar_bottom):
    if graph_type == "plot":
        matplotlib.pyplot.plot(x_list, y_list, label = key)
    elif graph_type == "bar":
        matplotlib.pyplot.bar(x_list, [x - datetime.datetime(1900, 1, 1) for x in y_list], bottom = bar_bottom, label = key)
        matplotlib.pyplot.ylim(bottom = bottom_limit, top = top_limit + datetime.timedelta(minutes = 10))
    elif graph_type == "stem":
        matplotlib.pyplot.stem(x_list, y_list, "", bottom = datetime.datetime.strptime("00", "%S"), basefmt = "", label = key)
        matplotlib.pyplot.ylim(bottom = bottom_limit, top = top_limit + datetime.timedelta(minutes = 10))
    elif graph_type == "scatter":
        matplotlib.pyplot.scatter(x_list, y_list, label = key)
    elif graph_type == "stairs":
        matplotlib.pyplot.stairs(y_list, linewidth = 2.5, label = key.replace("\"", ""), baseline = datetime.datetime.strptime("00", "%S"))
        matplotlib.pyplot.ylim(bottom = bottom_limit, top = top_limit + datetime.timedelta(minutes = 10))
    elif graph_type == "fill_between":
        matplotlib.pyplot.fill_between(x_list, y_list + (bar_bottom - datetime.datetime(1900, 1, 1)), bar_bottom, label = key)
        matplotlib.pyplot.ylim(bottom = bottom_limit, top = top_limit + datetime.timedelta(minutes = 10))

def get_stacked_bar_top_limit(log_file):
    y_list = []
    x_list = []
    for item in log_file:
        day = datetime.datetime.strptime(item.split(",")[0].split(" ")[0], "%Y-%m-%d")
        duration = datetime.datetime.strptime(item.split(",")[1], "%H:%M:%S.%f")
        if day in x_list:
            y_list[x_list.index(day)] += duration - datetime.datetime(1900, 1, 1)
        else:
            x_list.append(day)
            y_list.append(duration)

    return max(y_list)

def parse_log_file(log_file, stacked):
    x_list = []
    y_dict = {}

    for item in log_file:
        day = datetime.datetime.strptime(item.split(",")[0].split(" ")[0], "%Y-%m-%d")
        duration = datetime.datetime.strptime(item.split(",")[1], "%H:%M:%S.%f")

        if stacked:
            tag = item.split(",")[2]
        else:
            tag = "Time"

        if not tag in y_dict.keys():
            y_dict[tag] = []
        for key in y_dict:
            y_dict[key] = zero_filler(y_dict[key], len(x_list))

        if day in x_list:
            y_dict[tag][x_list.index(day)] += duration - datetime.datetime(1900, 1, 1)
        else:
            x_list.append(day)
            y_dict[tag].append(duration)

    for key in y_dict:
        y_dict[key] = zero_filler(y_dict[key], len(x_list))

    return (x_list, y_dict)

def show_graph(graph_type, x_grid, y_grid, stacked, legend):
    filename = "log.csv"
    log_file = list(map(str.strip, open(filename, "r", encoding="UTF-8").readlines()))
    log_file.pop(0) #remove header

    x_list, y_dict = parse_log_file(log_file, stacked)

    #matplotlib.rcParams["toolbar"] = "None" #disable toolbar
    matplotlib.pyplot.figure(num = "Time Logger Graph")
    matplotlib.pyplot.subplots_adjust(bottom=0.125, top=0.95, right = 0.975) #window<->plot percentage margins

    flattened_y_lists = []
    for key, y_list in y_dict.items():
        for y_item in y_list:
            if isinstance(y_item, datetime.date):
                flattened_y_lists.append(y_item)
    bottom_limit = min(flattened_y_lists)
    top_limit = max(flattened_y_lists)
    if stacked and (graph_type == "bar" or graph_type == "fill_between"):
        top_limit = get_stacked_bar_top_limit(log_file)
    bar_bottom = numpy.full(len(x_list), datetime.datetime.strptime("00", "%S"))

    for key, y_list in y_dict.items():
        setup_graph(graph_type, x_list, y_list, stacked, key.strip("\""), bottom_limit, top_limit, bar_bottom)
        bar_bottom += [x - datetime.datetime(1900, 1, 1) for x in y_list]

    if legend:
        matplotlib.pyplot.legend(loc = "upper right", draggable = True)
    if x_grid:
        matplotlib.pyplot.grid(visible = True, which = "both", axis = "x")
    if y_grid:
        matplotlib.pyplot.grid(visible = True, which = "major", axis = "y")

    matplotlib.pyplot.xlabel("Day (month-day)")
    matplotlib.pyplot.ylabel("Time (hours:minutes)")

    matplotlib.pyplot.title("Daily Time Graph")

    matplotlib.pyplot.gca().yaxis.set_major_formatter(matplotlib.dates.DateFormatter('%H:%M'))
    matplotlib.pyplot.yticks(numpy.arange(datetime.datetime.strptime("00", "%S"), top_limit, datetime.timedelta(minutes = 30)))

    matplotlib.pyplot.gca().xaxis.set_major_locator(matplotlib.dates.AutoDateLocator(maxticks = 14))
    matplotlib.pyplot.gca().xaxis.set_major_formatter(matplotlib.dates.DateFormatter('%m-%d'))
    matplotlib.pyplot.gca().xaxis.set_minor_locator(matplotlib.dates.DayLocator(interval = 1))
    matplotlib.pyplot.xticks(rotation = 20)

    matplotlib.pyplot.show()

if __name__ == "__main__":
    show_graph("bar", False, False, True, True)
