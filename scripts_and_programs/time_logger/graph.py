import matplotlib.pyplot
import matplotlib.dates
import numpy
import datetime

def show_graph(graph_type, x_grid, y_grid):
    filename = "log.csv"
    log_file = list(map(str.strip, open(filename, "r", encoding="UTF-8").readlines()))
    log_file.pop(0) #remove header

    x_list = []
    y_list = []

    for item in log_file:
        day = datetime.datetime.strptime(item.split(",")[0].split(" ")[0], "%Y-%m-%d")
        duration = datetime.datetime.strptime(item.split(",")[1], "%H:%M:%S.%f")
        if day in x_list:
            y_list[x_list.index(day)] += duration - datetime.datetime(1900, 1, 1)
        else:
            x_list.append(day)
            y_list.append(duration)

    #matplotlib.rcParams["toolbar"] = "None" #disable toolbar
    matplotlib.pyplot.figure(num = "Time Logger Graph")
    matplotlib.pyplot.subplots_adjust(bottom=0.125, top=0.95, right = 0.975) #window<->plot percentage margins

    if graph_type == "plot":
        matplotlib.pyplot.plot(x_list, y_list)
    elif graph_type == "bar":
        matplotlib.pyplot.bar(x_list, [x - datetime.datetime(1900, 1, 1) for x in y_list], bottom = datetime.datetime.strptime("00", "%S"))
        matplotlib.pyplot.ylim(bottom = min(y_list), top = max(y_list) + datetime.timedelta(minutes = 10))
    elif graph_type == "stem":
        matplotlib.pyplot.stem(x_list, y_list, bottom = datetime.datetime.strptime("00", "%S"), basefmt = "")
        matplotlib.pyplot.ylim(bottom = min(y_list), top = max(y_list) + datetime.timedelta(minutes = 10))
    elif graph_type == "scatter":
        matplotlib.pyplot.scatter(x_list, y_list)
    elif graph_type == "stairs":
        matplotlib.pyplot.stairs(y_list, linewidth = 2.5, baseline = datetime.datetime.strptime("00", "%S"))
        matplotlib.pyplot.ylim(bottom = min(y_list), top = max(y_list) + datetime.timedelta(minutes = 10))

    if x_grid:
        matplotlib.pyplot.grid(visible = True, which = "both", axis = "x")
    if y_grid:
        matplotlib.pyplot.grid(visible = True, which = "major", axis = "y")

    matplotlib.pyplot.xlabel("Day (month-day)")
    matplotlib.pyplot.ylabel("Time (hours:minutes)")

    matplotlib.pyplot.title("Daily Time Graph")

    matplotlib.pyplot.gca().yaxis.set_major_formatter(matplotlib.dates.DateFormatter('%H:%M'))
    matplotlib.pyplot.yticks(numpy.arange(datetime.datetime.strptime("00", "%S"), max(y_list), datetime.timedelta(minutes = 30)))

    matplotlib.pyplot.gca().xaxis.set_major_locator(matplotlib.dates.AutoDateLocator(maxticks = 14))
    matplotlib.pyplot.gca().xaxis.set_major_formatter(matplotlib.dates.DateFormatter('%m-%d'))
    matplotlib.pyplot.gca().xaxis.set_minor_locator(matplotlib.dates.DayLocator(interval = 1))
    matplotlib.pyplot.xticks(rotation = 20)

    matplotlib.pyplot.show()

if __name__ == "__main__":
    show_graph("plot", False, False)
