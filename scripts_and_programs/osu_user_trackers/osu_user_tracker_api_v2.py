import json
import requests
from datetime import datetime,timezone
import time
import re

try:
    API_TOKEN_PATH = "https://osu.ppy.sh/oauth/token/"
    client_id = ''
    client_secret = ''

    tokenParams = {"client_id" : client_id, "client_secret" : client_secret, "grant_type" : 'client_credentials' , "scope":'public'}
    token_request = requests.post(url=API_TOKEN_PATH, data=tokenParams)
    token = json.loads(token_request.text)

    headers = {"Authorization": token['token_type'] + " " + token['access_token']}
    
    with open("list.txt", "r") as userIDListRaw:
            userIDListLines = userIDListRaw.readlines()
    userIDList = list(map(str.strip, userIDListLines))
    for element in userIDList:
        userid = element
        url = "https://osu.ppy.sh/api/v2/users/" + userid + "/osu"
        url2 = "https://osu.ppy.sh/api/v2/users/" + userid + "/scores/" + "best"
        url3 = "https://osu.ppy.sh/api/v2/users/" + userid + "/scores/" + "recent"
        url4 = "https://osu.ppy.sh/api/v2/users/" + userid + "/scores/" + "firsts"
        url5 = "https://osu.ppy.sh/api/v2/users/" + userid + "/recent_activity"
        payload = {
            'key': "id",
        }
        payload2 = {
            'include_fails': 1,
            'mode': "osu",
            'limit': 999,
        }
        user_json = requests.get(url, params=payload, headers=headers)
        
        user_best_json = requests.get(url2, params=payload2, headers=headers)
        user_recent_json = requests.get(url3, params=payload2, headers=headers)
        user_firsts_json = requests.get(url4, params=payload2, headers=headers)
        user_activity_json = requests.get(url5, params=payload2, headers=headers)
    
        user = json.loads(user_json.text)
        user_best = json.loads(user_best_json.text)
        user_recent = json.loads(user_recent_json.text)
        user_firsts = json.loads(user_firsts_json.text)
        user_activity = json.loads(user_activity_json.text)

        errorcheck_1 = re.search("'error':", str(user))
        errorcheck_2 = re.search("'error':", str(user_best))
        errorcheck_3 = re.search("'error':", str(user_recent))
        errorcheck_4 = re.search("'error':", str(user_firsts))
        errorcheck_5 = re.search("'error':", str(user_activity))
        
        utc_time = datetime.now(timezone.utc).strftime("%Y-%m-%d_%H-%M-%S")
        with open(userid + '_info_v2_' + utc_time + '.json', 'w') as user_info_file:
            user_info_file.writelines(["User Info:\n\n"])
            json.dump(user, user_info_file)
            user_info_file.writelines(["\n\nUser Best:\n\n"])
            json.dump(user_best, user_info_file)
            user_info_file.writelines(["\n\nUser Recent:\n\n"])
            json.dump(user_recent, user_info_file)
            user_info_file.writelines(["\n\nUser Firsts:\n\n"])
            json.dump(user_firsts, user_info_file)
            user_info_file.writelines(["\n\nUser Activity:\n\n"])
            json.dump(user_activity, user_info_file)
        time.sleep(1)

        if errorcheck_1 is not None or errorcheck_2 is not None or errorcheck_3 is not None or errorcheck_4 is not None or errorcheck_5 is not None:
            print("Failed")
            try:
                print(user)
                print(user_best)
                print(user_recent)
                print(user_firsts)
                print(user_activity)
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
