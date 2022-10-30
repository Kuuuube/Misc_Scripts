import json
import requests
from datetime import datetime,timezone
import time
import re

try:
    
    with open("list.txt", "r") as userIDListRaw:
            userIDListLines = userIDListRaw.readlines()
    userIDList = list(map(str.strip, userIDListLines))
    for element in userIDList:
        userid = element
        url = "https://osu.ppy.sh/api/get_user"
        url2 = "https://osu.ppy.sh/api/get_user_best"
        url3 = "https://osu.ppy.sh/api/get_user_recent"
        payload = {
            'k': "",
            'u': userid,
            'type': 'id',
            'm': 0,  # osu! standard
            'limit': 100,
        }
        user_json = requests.get(url, params=payload)
        user_best_json = requests.get(url2, params=payload)
        user_recent_json = requests.get(url3, params=payload)
    
        user = json.loads(user_json.text)
        user_best = json.loads(user_best_json.text)
        user_recent = json.loads(user_recent_json.text)

        errorcheck_1 = re.search("'error':", str(user))
        errorcheck_2 = re.search("'error':", str(user_best))
        errorcheck_3 = re.search("'error':", str(user_recent))
        
        utc_time = datetime.now(timezone.utc).strftime("%Y-%m-%d_%H-%M-%S")
        with open(userid + '_info_' + utc_time + '.json', 'w') as user_info_file:
            user_info_file.writelines(["User Info:\n\n"])
            json.dump(user, user_info_file)
            user_info_file.writelines(["\n\nUser Best:\n\n"])
            json.dump(user_best, user_info_file)
            user_info_file.writelines(["\n\nUser Recent:\n\n"])
            json.dump(user_recent, user_info_file)
        time.sleep(1)

        if errorcheck_1 is not None or errorcheck_2 is not None or errorcheck_3 is not None:
            print("Failed")
            try:
                print(user)
                print(user_best)
                print(user_recent)
            except Exception as e:
                print (e)
                pass
            input()
        
        if userid == userIDList[-1]:
            break
    
except Exception as e:
    print("Failed")
    print (e)
    input()
    pass
