import re
import sys

args = sys.argv[1:]

pressure_hold_times_off = []
pressure_hold_times_on = []
pressure_time_deltas_off = []
pressure_time_deltas_on = []
skipped_lines = 0
filtered_lines = 0

data_capture_filename = args[0]
line_filter = args[1] if len(args) > 1 else ""

hold_timer_ms = 0
last_pressure = 0
with open(data_capture_filename) as tablet_data:
    for line in tablet_data.readlines():
        if line_filter in line:
            pressure_search = re.search(r"(?<=Pressure:)\d+", line)
            timedelta_search = re.search(r"(?<=Delta:)\d+\.?\d*", line)

            if not pressure_search or not timedelta_search:
                skipped_lines += 1
                continue

            pressure = int(pressure_search[0])
            timedelta = float(timedelta_search[0])

            if last_pressure > 0 and pressure == 0:
                pressure_time_deltas_off.append(timedelta)

                pressure_hold_times_on.append(hold_timer_ms)
                hold_timer_ms = 0

            if last_pressure == 0 and pressure > 0:
                pressure_time_deltas_on.append(timedelta)

                pressure_hold_times_off.append(hold_timer_ms)
                hold_timer_ms = 0

            hold_timer_ms += timedelta

            last_pressure = pressure
        else:
            filtered_lines += 1

pressure_hold_times_off.sort()
pressure_hold_times_on.sort()
pressure_time_deltas_off.sort()
pressure_time_deltas_on.sort()

splitter_line = "------------------------"

print(splitter_line + "\nPressure Hold Times Off\n" + splitter_line + "\n" + str(pressure_hold_times_off))
print(splitter_line + "\nPressure Hold Times On\n" + splitter_line + "\n" + str(pressure_hold_times_on))
print(splitter_line + "\nPressure Time Deltas Off\n" + splitter_line + "\n" + str(pressure_time_deltas_off))
print(splitter_line + "\nPressure Time Deltas On\n" + splitter_line + "\n" + str(pressure_time_deltas_on))

print(splitter_line)

print("Pressure On Delta: (" + "Min: " + str(min(pressure_time_deltas_on)) + "ms, Max: " + str(max(pressure_time_deltas_on)) + "ms, Average: " + str(sum(pressure_time_deltas_on) / len(pressure_time_deltas_on)) + "ms)")
print("Pressure Off Delta: (" + "Min: " + str(min(pressure_time_deltas_off)) + "ms, Max: " + str(max(pressure_time_deltas_off)) + "ms, Average: " + str(sum(pressure_time_deltas_off) / len(pressure_time_deltas_off)) + "ms)")

print("Min hold time on: " + str(min(pressure_hold_times_on)) + "ms")
print("Min hold time off: " + str(min(pressure_hold_times_off)) + "ms")

print(splitter_line)

print("Skipped " + str(skipped_lines) + " lines. Filtered " + str(filtered_lines) + " lines.")
