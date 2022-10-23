start_number = input("Start Number: ")
end_number = input("End Number: ")

number_prefix = input("Number Prefix: ")
number_suffix = input("Number Suffix: ")

current_number = start_number
numberstring = ""

while int(current_number) <= int(end_number):
    numberstring += (str(number_prefix) + str(current_number) + str(number_suffix))
    numberstring += ("\n")
    current_number = int(current_number) + 1

with open ("output.txt", "w") as numfile:
    numfile.write(numberstring)
