import re
import os

def id_then_name():
    with open ("sheet_list.txt", "r", encoding="UTF-8") as SheetListRaw:
        SheetListLines = SheetListRaw.readlines()
        SheetList = list(map(str.strip, SheetListLines))

        if os.path.isdir("files_to_read"):
            pass
        else:
            os.mkdir("files_to_read")
        
        for element in SheetList:
            name = re.search("(?<=<\|\|>).*", element)
            item = re.search(".*(?=<\|\|>)", element)

            if name != None and item != None:
                with open ("files_to_read\\" + name.group(0), "a") as SheetSplit:
                    SheetSplit.write(item.group(0))
                    SheetSplit.write("\n")
                    print(item.group(0))

def name_then_id():
    with open ("sheet_list.txt", "r") as SheetListRaw:
        SheetListLines = SheetListRaw.readlines()
        SheetList = list(map(str.strip, SheetListLines))

        if os.path.isdir("files_to_read"):
            pass
        else:
            os.mkdir("files_to_read")
        
        for element in SheetList:
            name = re.search(".*(?=<\|\|>)", element)
            item = re.search("(?<=<\|\|>).*", element)

            if name != None and item != None:
                with open ("files_to_read\\" + name.group(0), "a") as SheetSplit:
                    SheetSplit.write(item.group(0))
                    SheetSplit.write("\n")
                    print(item.group(0))
