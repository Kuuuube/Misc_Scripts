import os
import list_mapid_info_puller
import setID_to_mapIDs
import html_to_list

api_key = ""
file_list = os.listdir("files_to_read")

for element in file_list:
    setID_to_mapIDs.setID_to_list("files_to_read\\" + element)
    html_to_list.html_to_list("files_to_read\\" + element)
    setID_to_mapIDs.setID_list_to_mapID_list(api_key)

    collection_path = element + ".osdb"
    try:
        list_mapid_info_puller.id_to_db(api_key, collection_path)
        with open ("log.txt", "a", encoding='utf8') as log_file:
            log_file.write(str(element))
            log_file.write("\n")
    except Exception as e:
        with open ("log.txt", "a", encoding='utf8') as log_file:
            log_file.write(str(element) + " Failed")
            log_file.write("\n")
            log_file.write(str(e))
            log_file.write("\n")
        print(e)
        pass
