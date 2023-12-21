file_lines = list(map(str.strip, open("list.tsv", "r", encoding="UTF-8").readlines()))

freq_rank = 0
last_freq = int(file_lines[0].split("\t")[1]) + 1 #1 higher than the first row count
sorted_list = []
for line in file_lines:
    line_split = line.split("\t")
    print(int(line_split[1]), last_freq)
    if int(line_split[1]) < last_freq:
        freq_rank += 1
        last_freq = int(line_split[1])
        sorted_list.append((line_split[0],str(freq_rank)))
    elif int(line_split[1]) == last_freq:
        sorted_list.append((line_split[0],str(freq_rank)))
    else:
        #this shouldnt happen
        print("sort your frequency list before passing it through")


with open("term_meta_bank_1.json", "w", encoding = "UTF-8") as results_file:
    results_file.write("[")
    list_length = len(sorted_list)
    for i, item in enumerate(sorted_list):
        results_file.write("[\"" + item[0] + "\",\"freq\",{\"value\":" + item[1] + ",\"displayValue\":\"" + item[1] + "\"}]")
        if i < list_length - 1:
            results_file.write(",")
    results_file.write("]")
