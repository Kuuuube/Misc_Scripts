import re
import osu_collector_dumper
import requests
import json
import traceback

try:
    html_check = "0"
    osu_collector_check = "0"
    osu_collector_check = "Y"

    numbers_list = list(map(str.strip, open("list.txt", "r", encoding="UTF-8").readlines()))

    for current_number in numbers_list:
        try:
            print("Dumping: " + str(current_number))
            collection_id = current_number
            collection_path = current_number
            if re.search("(.(db|osdb))", collection_path) == None:
                collection_path = re.sub("$", ".osdb", collection_path)
            osu_collector_dumper.osu_collector_dump_v2_endpoint(collection_id, collection_path)
            current_number = str(int(current_number) + 1)
        except Exception:
            current_number = str(int(current_number) + 1)
            print("Failed")
            print (traceback.format_exc())
            with open ("errorlog.txt", "a") as error_log:
                error_log.write("Failed: " + str(int(current_number) - 1) + " Error: ")
                error_log.write(traceback.format_exc())
                error_log.write("\n")
            pass
        
except Exception:
    print("Failed")
    print (traceback.format_exc())
    input()
    pass
