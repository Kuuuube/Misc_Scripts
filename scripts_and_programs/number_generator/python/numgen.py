start_number = input("Start Number: ")
end_number = int(input("End Number: "))

number_prefix = input("Number Prefix: ")
number_suffix = input("Number Suffix: ")

current_number = int(start_number)

with open ("output.txt", "w") as numfile:
    while current_number <= end_number:
        numfile.write(number_prefix + str(current_number) + number_suffix + "\n")
        current_number += 1
