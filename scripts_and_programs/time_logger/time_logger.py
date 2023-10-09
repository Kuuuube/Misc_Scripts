import timeit
import datetime

input("Time stopped, press enter to start.")

while True:
    start_time = timeit.default_timer()
    input("Time started press enter to stop.")

    with open("log.csv", "a") as logfile:
        logfile.write(str(datetime.datetime.now()))
        logfile.write("\t")
        logfile.write(str(timeit.default_timer() - start_time))
        logfile.write("\n")

    input("Time stopped, press enter to start.")
        
