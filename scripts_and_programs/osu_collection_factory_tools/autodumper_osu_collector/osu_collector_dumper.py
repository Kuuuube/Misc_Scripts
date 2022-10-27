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
    try:
        collection = json.loads(r.text)
    except Exception as e:
        print(str(collection_id) + " Bad json")
        with open ("errorlog.txt", "a") as error_log:
            error_log.write("Failed: " + str(collection_id) + " Bad Json. Error: ")
            error_log.write(str(e))
            error_log.write("\n")
        raise ValueError("This error is used to kill the module in case of a bad json. See the previous error for what happened.")

    try:
        sets_json = collection['beatmapsets']
    except Exception:
        print("Bad json")

    with open (csv_filepath, "a", encoding="utf8") as osdb_data_dump:
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

    try:
        username = collection['uploader']['username']
    except Exception:
        username = ""

    try:
        name = collection['name']
    except Exception:
        name = ""
    
    try:
        #python is awful so "\\\\n" is required to write a literal "\n" instead of a newline
        description = re.sub("(\n|\r\n)", "\\\\n", collection['description'])
    except Exception:
        description = ""
    
    try:
        beatmapcount = collection['beatmapCount']
    except Exception:
        beatmapcount = ""
    
    mapped_string = "CollectionID: " + str(collection_id_regex.group(0)) + ", Collection Name: " + name + ", Collection Description: " + description + ", Beatmapcount: " + str(beatmapcount) + ", Uploader: " + username

    print (mapped_string)
        
    with open (metdata_filepath, "a", encoding="utf8") as id_dump:
        id_dump.writelines([mapped_string])
        id_dump.writelines(["\n"])

    print ("osu!Collectior: waiting.")
    time.sleep(5)

def osu_collector_dump_v2_endpoint(collection_id, collection_path):

    collection_path_no_extension_1 = re.sub(".*(\\\|\\\\)", "", collection_path)
    collection_path_no_extension_2 = re.sub("\..*", "", collection_path_no_extension_1)

    collection_id_regex = re.search("\d*$", collection_id)

    hasMore = True
    cursor = "0"
    csv_filepath = 'CollectionConverter\\' + collection_path_no_extension_2 + '.csv'

    with open(csv_filepath, 'w') as id_dump:
        id_dump.close()

    while hasMore == True:

        url = "https://osucollector.com/api/collections/" + collection_id_regex.group(0) + "/beatmapsv2?"
        payload = {
            "cursor": cursor,
            "perPage": "100",
            "sortBy": "difficulty_rating",
        }

        r = requests.get(url, payload)
        try:
            collection = json.loads(r.text)
        except Exception as e:
            print(str(collection_id) + " Bad json")
            with open ("errorlog.txt", "a") as error_log:
                error_log.write("Failed: " + str(collection_id) + " Bad Json. Error: ")
                error_log.write(str(e))
                error_log.write("\n")
            raise ValueError("This error is used to kill the module in case of a bad json. See the previous error for what happened.")

        try:
            maps_json = collection['beatmaps']
        except Exception:
            print("Bad json")

        print ("osu!Collectior: waiting.")
        print (cursor)

        cursor = collection["nextPageCursor"]
        hasMore = collection["hasMore"]

        with open (csv_filepath, "a", encoding="utf8") as osdb_data_dump:
            try:
                for Maps in maps_json:
                    osdb_data_dump.write("\"" + collection_id_regex.group(0) + "\",\"" + str(Maps['id']) + "\",\"" + str(Maps['beatmapset']['id']) + "\",\"" + str(Maps['checksum']) + "\",\"" + str(resolve_mode(Maps['mode_int'])) + "\",\"" + str(Maps['beatmapset']['artist']) + "\",\"" + str(Maps['beatmapset']['artist_unicode']) + "\",\"" + str(Maps['beatmapset']['title']) + "\",\"" + str(Maps['beatmapset']['title_unicode']) + "\",\"" + str(Maps['version']) + "\",\"" + str(Maps['difficulty_rating']) + "\"")
                    osdb_data_dump.write('\n')
            except Exception as e:
                print (e)
                print("No maps found")
                pass

        if hasMore == True:
            time.sleep(5)

    metadata_dumper(collection_id)

    subprocess.check_call([r"CollectionConverter\CollectionConverter.exe", csv_filepath, collection_path, "3", "2", "0"])

    print ("osu!Collectior: waiting.")
    time.sleep(5)

def resolve_mode(mode_int):
    if (mode_int == 0):
        return "Osu"
    if (mode_int == 1):
        return "Taiko"
    if (mode_int == 2):
        return "CatchTheBeat"
    if (mode_int == 3):
        return "OsuMania"

def metadata_dumper(collection_id):

    collection_id_regex = re.search("\d*$", collection_id)

    url = "https://osucollector.com/api/collections/" + collection_id_regex.group(0)

    r = requests.get(url)
    try:
        collection = json.loads(r.text)
    except Exception as e:
        print(str(collection_id) + " Bad json")
        with open ("errorlog.txt", "a") as error_log:
            error_log.write("Failed: " + str(collection_id) + " Bad Json. Error: ")
            error_log.write(str(e))
            error_log.write("\n")
        raise ValueError("This error is used to kill the module in case of a bad json. See the previous error for what happened.")

    metdata_filepath = "metadata_list.txt"

    try:
        username = collection['uploader']['username']
    except Exception:
        username = ""

    try:
        name = collection['name']
    except Exception:
        name = ""
    
    try:
        #python is awful so "\\\\n" is required to write a literal "\n" instead of a newline
        description = re.sub("(\n|\r\n)", "\\\\n", collection['description'])
    except Exception:
        description = ""

    try:
        beatmapcount = collection['beatmapCount']
    except Exception:
        beatmapcount = ""

    mapped_string = "CollectionID: " + str(collection_id_regex.group(0)) + ", Collection Name: " + name + ", Collection Description: " + description + ", Beatmapcount: " + str(beatmapcount) + ", Uploader: " + username
    
    print (mapped_string)
        
    with open (metdata_filepath, "a", encoding="utf8") as id_dump:
        id_dump.writelines([mapped_string])
        id_dump.writelines(["\n"])
