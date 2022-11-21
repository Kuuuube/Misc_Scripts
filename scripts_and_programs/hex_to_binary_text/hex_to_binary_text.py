print("Enter input file path:")
inputfilepath = input()

print("Enter output file path:")
outputfilepath = input()

with open (inputfilepath, "r", encoding="UTF-8") as inputfileRaw:
        inputfileLines = inputfileRaw.readlines()
        inputfile = list(map(str.strip, inputfileLines))

with open (outputfilepath, "w", encoding="UTF-8") as output:
    for line in inputfile:
        output.write(line.replace("0","0000").replace("1","0001").replace("2","0010").replace("3","0011").replace("4","0100").replace("5","0101").replace("6","0110").replace("7","0111").replace("8","1000").replace("9","1001").replace("A","1010").replace("B","1011").replace("C","1100").replace("D","1101").replace("E","1110").replace("F","1111"))
        output.write("\n")
