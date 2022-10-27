import requests
import json
import re
import time
import subprocess

def osu_collector_dump(collection_id, collection_path):
    
    collection_path_no_extension_1 = re.sub(".*(\\\|\\\\)", "", collection_path)
    collection_path_no_extension_2 = re.sub("\..*", "", collection_path_no_extension_1)
    
    collection_id_regex = re.search("\d*$", collection_id)
    
    csv_filepath = 'CollectionConverter\\' + collection_path_no_extension_2 + '.csv'

    with open(csv_filepath, 'w') as id_dump:
        id_dump.close()
    
    url = "https://osucollector.com/api/collections/" + collection_id_regex.group(0)

    r = requests.get(url)
    collection = json.loads(r.text)

    print ("osu!Collectior: waiting.")

    try:
        sets_json = collection['beatmapsets']
    except Exception:
        print("Bad json")

    with open (csv_filepath, "a") as osdb_data_dump:
        try:
            for MapSets in sets_json:
                for Maps in MapSets['beatmaps']:
                    osdb_data_dump.write("\"" + collection_id_regex.group(0) + "\",\"" + str(Maps['id']) + "\",\"" + str(MapSets['id']) + "\",\"" + str(Maps['checksum']) + "\",\"" + "" + "\",\"" + "" + "\",\"" + "" + "\",\"" + "" + "\",\"" + "" + "\",\"" + "" + "\",\"" + "" + "\"")
                    osdb_data_dump.write('\n')
        except Exception:
            print("No maps found")
            pass

    subprocess.check_call([r"CollectionConverter\CollectionConverter.exe", csv_filepath, collection_path, "3", "2", "0"])

    #metadata dumper tacked on:
    
    metdata_filepath = "metadata_list.txt"

    #uploaderstats = re.search('"uploader": {.*?}', json.dumps(collection))
    try:
        username = collection['uploader']['username']
    except Exception:
        username = "None"

    try:
        name = collection['name']
    except Exception:
        name = "None"
    
    try:
        description = collection['description']
    except Exception:
        description = "None"
    
    try:
        beatmapcount = collection['beatmapCount']
    except Exception:
        beatmapcount = "None"
    
    mapped_string = "CollectionID: " + str(collection_id_regex.group(0)) + ", Collection Name: " + str(name) + ", Collection Description: " + str(description) + ", Beatmapcount: " + str(beatmapcount) + ", Uploader: " + str(username)

    print (mapped_string)
        
    with open (metdata_filepath, "a") as id_dump:
        id_dump.writelines([mapped_string])
        id_dump.writelines(["\n"])

    time.sleep(5)