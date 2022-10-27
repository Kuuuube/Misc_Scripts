import re
import osu_collector_dumper
import requests
import json

try:
    html_check = "0"
    osu_collector_check = "0"
    osu_collector_check = "Y"

    with open("start_number.txt", "r") as start_number_file:
        first_line = start_number_file.readline()
        start_number = str(int(first_line) + 1)


    print(start_number)

    check_latest = requests.get(url="https://osucollector.com/api/collections/recent?cursor=1&perPage=1")
    #regex_id_find = re.search('(?<=,"id":)\d{1,5}', check_latest.text)
    #end_number = regex_id_find.group(0)
    json_text = json.loads(check_latest.text)
    json1 = json_text['collections']
    json2 = json1[0]
    json3 = json2['id']
    end_number = json3

    print(end_number)

    if int(start_number) > int(end_number):
        print ("There are no collections to download or something broke")
        input()

    current_number = start_number
        
    while not int(current_number) > (int(end_number)):
        try:
            collection_id = current_number
            collection_path = current_number
            if re.search("(.(db|osdb))", collection_path) == None:
                collection_path = re.sub("$", ".osdb", collection_path)
            osu_collector_dumper.osu_collector_dump(collection_id, collection_path)
            current_number = str(int(current_number) + 1)
        except Exception as e:
            current_number = str(int(current_number) + 1)
            print("Failed")
            print (e)
            with open ("errorlog.txt", "a") as error_log:
                error_log.write("Failed: " + str(int(current_number) - 1) + " Error: ")
                error_log.write(str(e))
                error_log.write("\n")
            pass

    with open("start_number.txt", "w") as end_number_file:
        end_number_file.writelines([str(end_number)])
        
except Exception as e:
    print("Failed")
    print (e)
    input()
    pass
