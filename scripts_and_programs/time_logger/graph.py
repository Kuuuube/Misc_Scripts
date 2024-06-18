import matplotlib.pyplot
import matplotlib.dates
import numpy
import datetime
import operator

def zero_filler(input_list, target_len):
    while len(input_list) < target_len:
        input_list.append(datetime.datetime(1900, 1, 1))
    return input_list

def setup_graph(graph_type, x_list, y_list, stacked, key, bottom_limit, top_limit, bar_bottom):
    if graph_type == "plot":
        matplotlib.pyplot.plot(x_list, y_list, label = key)
        matplotlib.pyplot.ylim(bottom = bottom_limit, top = top_limit + datetime.timedelta(minutes = 10))
    elif graph_type == "bar":
        matplotlib.pyplot.bar(x_list, [x - datetime.datetime(1900, 1, 1) for x in y_list], bottom = bar_bottom, label = key)
        matplotlib.pyplot.ylim(bottom = bottom_limit, top = top_limit + datetime.timedelta(minutes = 10))
    elif graph_type == "stem":
        matplotlib.pyplot.stem(x_list, y_list, "", bottom = datetime.datetime.strptime("00", "%S"), basefmt = "", label = key)
        matplotlib.pyplot.ylim(bottom = bottom_limit, top = top_limit + datetime.timedelta(minutes = 10))
    elif graph_type == "scatter":
        matplotlib.pyplot.scatter(x_list, y_list, label = key)
        matplotlib.pyplot.ylim(bottom = bottom_limit, top = top_limit + datetime.timedelta(minutes = 10))
    elif graph_type == "stairs":
        matplotlib.pyplot.stairs(y_list, x_list + [max(x_list) + datetime.timedelta(days = 1)], linewidth = 2.5, label = key.replace("\"", ""), baseline = datetime.datetime.strptime("00", "%S"))
        matplotlib.pyplot.ylim(bottom = bottom_limit, top = top_limit + datetime.timedelta(minutes = 10))
    elif graph_type == "fill_between":
        matplotlib.pyplot.fill_between(x_list, y_list + (bar_bottom - datetime.datetime(1900, 1, 1)), bar_bottom, label = key)
        matplotlib.pyplot.ylim(bottom = bottom_limit, top = top_limit + datetime.timedelta(minutes = 10))

def add_total(flattened_y_lists, graph_total_label):
    total = datetime.timedelta()
    for value in flattened_y_lists:
        total += value - datetime.datetime(1900, 1, 1)
    hours_string = str(total.seconds // 3600 + total.days * 24)
    matplotlib.pyplot.title(graph_total_label.replace("%d", hours_string), loc = 'right')

def parse_log_file(log_file, stacked, day_offset):
    x_list = []
    y_dict = {}

    for item in log_file:
        raw_day = datetime.datetime.strptime(item.split(",")[0], "%Y-%m-%d %H:%M:%S.%f")
        day = (raw_day + datetime.timedelta(hours = day_offset)).replace(hour=0, minute=0, second=0, microsecond=0)
        duration = datetime.datetime.strptime(item.split(",")[1], "%H:%M:%S.%f")

        if stacked:
            tag = item.split(",")[2]
        else:
            tag = "Time"

        #days with no logs are recorded as having zero time instead of being ignored to fix plotting issues on some graph types
        if len(x_list) > 0 and day - x_list[-1] > datetime.timedelta(days = 1):
            for _ in range((day - x_list[-1]).days - 1):
                new_day = x_list[-1] + datetime.timedelta(days = 1)
                x_list.append(new_day)
                if not tag in y_dict.keys():
                    y_dict[tag] = []
                y_dict[tag] = zero_filler(y_dict[tag], len(x_list))
                y_dict[tag][x_list.index(new_day)] = datetime.datetime(1900, 1, 1)

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

def show_graph(graph_type, x_grid, y_grid, stacked, legend, graph_total_label, csv_has_header, day_offset, max_display_months):
    filename = "log.csv"
    log_file = list(map(str.strip, open(filename, "r", encoding="UTF-8").readlines()))
    if csv_has_header:
        log_file.pop(0) #remove header

    x_list, y_dict = parse_log_file(log_file, stacked, day_offset)

    #matplotlib.rcParams["toolbar"] = "None" #disable toolbar
    matplotlib.pyplot.figure(num = "Time Logger Graph")
    matplotlib.pyplot.subplots_adjust(bottom=0.125, top=0.95, right = 0.975) #window<->plot percentage margins

    flattened_y_lists = []
    for key, y_list in y_dict.items():
        if len(flattened_y_lists) == 0:
            flattened_y_lists = y_list
        else:
            duration_list = []
            for item in y_list:
                duration_list.append(item - datetime.datetime(1900, 1, 1))
            flattened_y_lists = list(map(operator.add, flattened_y_lists, duration_list))
    filtered_y_list = []
    for x_item, y_item in zip(x_list, flattened_y_lists):
        if x_item > x_list[-1] - datetime.timedelta(days = max_display_months * 30):
            filtered_y_list.append(y_item)

    bottom_limit = min(flattened_y_lists)
    top_limit = max(filtered_y_list)

    bar_bottom = numpy.full(len(x_list), datetime.datetime.strptime("00", "%S"))

    for key, y_list in y_dict.items():
        setup_graph(graph_type, x_list, y_list, stacked, key.strip("\"").replace("\"", "\"\""), bottom_limit, top_limit, bar_bottom)
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

    if (graph_total_label):
        add_total(flattened_y_lists, graph_total_label)

    if x_list[-1] - x_list[0] > datetime.timedelta(days = max_display_months * 30):
        matplotlib.pyplot.gca().set_xlim(xmin = x_list[-1] - datetime.timedelta(days = max_display_months * 30))
    else:
        matplotlib.pyplot.gca().set_xlim(xmin = x_list[0] - datetime.timedelta(days = 1))
    matplotlib.pyplot.gca().set_xlim(xmax = x_list[-1] + datetime.timedelta(days = 1))

    matplotlib.pyplot.gca().yaxis.set_major_formatter(matplotlib.dates.DateFormatter('%H:%M'))
    matplotlib.pyplot.yticks(numpy.arange(datetime.datetime.strptime("00", "%S"), top_limit, datetime.timedelta(minutes = 30)))

    matplotlib.pyplot.gca().xaxis.set_major_locator(matplotlib.dates.AutoDateLocator(maxticks = 14))
    matplotlib.pyplot.gca().xaxis.set_major_formatter(matplotlib.dates.DateFormatter('%m-%d'))
    matplotlib.pyplot.gca().xaxis.set_minor_locator(matplotlib.dates.DayLocator(interval = 1))
    matplotlib.pyplot.xticks(rotation = 20)

    matplotlib.pyplot.show()

if __name__ == "__main__":
    #show_graph(graph_type, x_grid, y_grid, stacked, legend, graph_total_label, csv_has_header, day_offset, max_display_months)
    show_graph("bar", False, False, True, True, "Total: %d hours", True, -4, 4)
