unicode_ranges = [
    ["3400", "4DBF"], # CJK Unified Ideographs Extension A
    ["4E00", "9FFF"], # CJK Unified Ideographs
    ["F900", "FAFF"], # CJK Compatibility Ideographs
    ["20000", "2A6DF"], # CJK Unified Ideographs Extension B
    ["2A700", "2B73F"], # CJK Unified Ideographs Extension C
    ["2B740", "2B81F"], # CJK Unified Ideographs Extension D
    ["2B820", "2CEAF"], # CJK Unified Ideographs Extension E
    ["2CEB0", "2EBEF"], # CJK Unified Ideographs Extension F
    ["2EBF0", "2EE5F"], # CJK Unified Ideographs Extension I
    ["2F800", "2FA1F"], # CJK Compatibility Ideographs Supplement
    ["30000", "3134F"], # CJK Unified Ideographs Extension G
    ["31350", "323AF"] # CJK Unified Ideographs Extension H
    ]

delimiter = "\n"

characters = []
for unicode_range in unicode_ranges:
    unicode_block_start = unicode_range[0]
    unicode_block_end = unicode_range[1]
    unicode_character_range = range(int(unicode_block_start, 16), int(unicode_block_end, 16) + 1)
    print("Generating " + str(len(unicode_character_range)) + " characters from range " + unicode_block_start + "-" + unicode_block_end + ".")
    for i in unicode_character_range:
        characters.append(chr(i) + delimiter)

filename = ""
for unicode_range in unicode_ranges:
    filename += unicode_range[0] + "-" + unicode_range[1] + "_"

filename = filename[:-1] + ".txt" #[:-1] to remove last char in string
    

with open(filename, "w", encoding="utf-8") as unicode_range_file:
    unicode_range_file.write("".join(characters))

print("Wrote " + str(len(characters)) + " unicode characters from " + str(len(unicode_ranges)) + " unicode ranges.")
