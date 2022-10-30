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
        payload = {
            'k': "",
            'u': userid,
            'type': 'id',
            'm': 0,  # osu! standard
        }
        user_json = requests.get(url, params=payload)
        user = json.loads(user_json.text)
        errorcheck_1 = re.search("'error':", str(user))

        username = re.search("(?<='username': ').*?(?=')",str(user))

        url2 = "https://osuskills.com/user/" + username.group(0)

        headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/98.0.4758.102 Safari/537.36',
        }
 
        osuskills = requests.get(url2, headers=headers)

        utc_time = datetime.now(timezone.utc).strftime("%Y-%m-%d_%H-%M-%S")
        with open (userid + '_osuskills_' + utc_time + '.html','w') as osuskills_dump:
            osuskills_dump.write(osuskills.text)
            
        time.sleep(1)
        if errorcheck_1 is not None:
            print("Failed")
            try:
                print(osuskills)
                print(osuskills.text)
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
