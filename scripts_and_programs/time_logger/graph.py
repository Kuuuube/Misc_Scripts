import matplotlib.pyplot
import matplotlib.dates
import numpy
import datetime

filename = "log.csv"
log_file = list(map(str.strip, open(filename, "r", encoding="UTF-8").readlines()))


x_list = []
y_list = []

log_file.pop(0)

for item in log_file:
    day = datetime.datetime.strptime(item.split(",")[0].split(" ")[0], "%Y-%m-%d")
    duration = datetime.datetime.strptime(item.split(",")[1], "%H:%M:%S.%f")
    if day in x_list:
        y_list[x_list.index(day)] += duration - datetime.datetime(1900, 1, 1)
    else:
        x_list.append(day)
        y_list.append(duration)

matplotlib.pyplot.plot(x_list, y_list)
matplotlib.pyplot.xlabel("Day (month-day)")
matplotlib.pyplot.ylabel("Time (hours:minutes)")

matplotlib.pyplot.title("Daily Time Graph")

matplotlib.pyplot.gca().yaxis.set_major_formatter(matplotlib.dates.DateFormatter('%H:%M'))
matplotlib.pyplot.yticks(numpy.arange(datetime.datetime.strptime("00", "%S"), max(y_list), datetime.timedelta(minutes = 30)))

matplotlib.pyplot.gca().xaxis.set_major_locator(matplotlib.dates.AutoDateLocator(maxticks = 14))
matplotlib.pyplot.gca().xaxis.set_major_formatter(matplotlib.dates.DateFormatter('%m-%d'))
matplotlib.pyplot.gca().xaxis.set_minor_locator(matplotlib.dates.DayLocator(interval = 1))
matplotlib.pyplot.xticks(rotation = 20)

matplotlib.pyplot.grid(which = "both", axis = "x")
matplotlib.pyplot.grid(which = "major", axis = "y")

matplotlib.pyplot.show()
