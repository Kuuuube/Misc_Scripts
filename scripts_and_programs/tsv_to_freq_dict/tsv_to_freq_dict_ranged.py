file_lines = list(map(str.strip, open("list.tsv", "r", encoding="UTF-8").readlines()))

print("Started processing")

def generate_freq_ranges_dict(file_lines):
    freq_rank = 1
    last_freq = int(file_lines[0].split("\t")[1]) + 1 #1 higher than the first row count
    freq_ranges = {}
    for line in file_lines:
        line_split = line.split("\t")
        if int(line_split[1]) < last_freq:
            if freq_rank in freq_ranges.keys():
                freq_rank = freq_ranges[freq_rank][1] + 1
            last_freq = int(line_split[1])
            freq_ranges[freq_rank] = (freq_rank, freq_rank)
        elif int(line_split[1]) == last_freq:
            freq_ranges[freq_rank] = (freq_ranges[freq_rank][0],freq_ranges[freq_rank][1] + 1)
        else:
            #this shouldnt happen
            print("sort your frequency list before passing it through")

    sorted_freq_ranges = {}
    for i, freq in enumerate(freq_ranges.values()):
        sorted_freq_ranges[i + 1] = freq

    return sorted_freq_ranges

freq_ranges_dict = generate_freq_ranges_dict(file_lines) #dictionary to convert base frequency into ranged frequency

freq_rank = 0
last_freq = int(file_lines[0].split("\t")[1]) + 1 #1 higher than the first row count
sorted_list = []

for line in file_lines:
    line_split = line.split("\t")
    if int(line_split[1]) < last_freq:
        freq_rank += 1
        last_freq = int(line_split[1])
        sorted_list.append((line_split[0],str(freq_rank)))
    elif int(line_split[1]) == last_freq:
        sorted_list.append((line_split[0],str(freq_rank)))
    else:
        #this shouldnt happen
        print("sort your frequency list before passing it through")
    print(line_split[0] + ":" + str(freq_rank))

print("Finished processing\nSaving file")

with open("term_meta_bank_1.json", "w", encoding = "UTF-8") as results_file:
    results_file.write("[")
    list_length = len(sorted_list)
    for i, item in enumerate(sorted_list):
        freq_range = (freq_ranges_dict[int(item[1])][0], freq_ranges_dict[int(item[1])][1])
        if freq_range[0] == freq_range[1]:
            results_file.write("[\"" + item[0] + "\",\"freq\",{\"value\":" + str(freq_range[0]) + ",\"displayValue\":\"" + str(freq_range[0]) + "\"}]")
        else:
            results_file.write("[\"" + item[0] + "\",\"freq\",{\"value\":" + str(freq_range[0]) + ",\"displayValue\":\"" + str(freq_range[0]) + "-" + str(freq_range[1]) + "\"}]")
        if i < list_length - 1:
            results_file.write(",")
    results_file.write("]")

print("Done")
