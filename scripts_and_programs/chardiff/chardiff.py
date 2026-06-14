import sys

file_a = sys.argv[1]
file_b = sys.argv[2]

file_string_a = open(file_a, "r", encoding="UTF-8").read()
file_string_b = open(file_b, "r", encoding="UTF-8").read()

only_a = set()
only_b = set()
both = set()

for char in file_string_a:
    if char not in file_string_b:
        only_a.add(char)
    else:
        both.add(char)

for char in file_string_b:
    if char not in file_string_a:
        only_b.add(char)
    else:
        both.add(char)

print("Found " + str(len(only_a)) + " characters only in `" + file_a + "`:")
print("".join(only_a))
print("Found " + str(len(only_b)) + " characters only in `" + file_b + "`:")
print("".join(only_b))
print("Found " + str(len(both)) + " characters present in both files:")
print("".join(both))
