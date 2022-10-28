import json
import requests
import re
import time
import subprocess

def id_to_db(api_key, collection_path):
    collection_path_no_extension_1 = re.sub(".*(\\\|\\\\)", "", collection_path)
    collection_path_no_extension_2 = re.sub("\.[^\.]*$", "", collection_path_no_extension_1)
    csv_filepath = 'CollectionConverter\\' + collection_path_no_extension_2 + '.csv'

    with open(csv_filepath, 'w') as beatmap_info_file:
        beatmap_info_file.close()
    
    with open("list.txt", "r") as beatmapIDListRaw:
        beatmapIDListLines = beatmapIDListRaw.readlines()
    beatmapIDList = list(map(str.strip, beatmapIDListLines))

    with open(csv_filepath, 'a', encoding="utf8") as beatmap_info_file:
        for element in beatmapIDList:
            beatmapid = element
            url = "https://osu.ppy.sh/api/get_beatmaps"
            payload = {
                'k': api_key,
                'b': beatmapid,
                'type': 'id',
                'limit': 100,
            }
            if len(beatmapid) > 0:
                r = requests.get(url, params=payload)
                beatmap = json.loads(r.text)

                for Maps in beatmap:
                    beatmap_info_file.write("\"" + collection_path_no_extension_2 + "\",\"" + str(Maps['beatmap_id']) + "\",\"" + str(Maps['beatmapset_id']) + "\",\"" + str(Maps['file_md5']) + "\",\"" + str(resolve_mode(Maps['mode'])) + "\",\"" + str(Maps['artist']) + "\",\"" + str(Maps['artist_unicode']) + "\",\"" + str(Maps['title']) + "\",\"" + str(Maps['title_unicode']) + "\",\"" + str(Maps['version']) + "\",\"" + str(Maps['difficultyrating']) + "\"")
                    beatmap_info_file.write("\n")
                    print ("ID: " + beatmapid + " MD5: " + str(Maps['file_md5']))

                time.sleep(1)

    subprocess.check_call([r"CollectionConverter\CollectionConverter.exe", csv_filepath, collection_path, "3", "2", "0"])


def resolve_mode(mode_int):
    if (mode_int == 0):
        return "Osu"
    if (mode_int == 1):
        return "Taiko"
    if (mode_int == 2):
        return "CatchTheBeat"
    if (mode_int == 3):
        return "OsuMania"

def nullcheck(value):
    if value == None:
        value = ""