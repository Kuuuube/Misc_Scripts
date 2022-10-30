import requests
import time

try:
    
    with open("list.txt", "r") as userIDListRaw:
            userIDListLines = userIDListRaw.readlines()
    userIDList = list(map(str.strip, userIDListLines))
    for element in userIDList:
        userid = element
        url = "https://osutrack-api.ameo.dev/update?user=" + userid + "&mode=0"

        osutrack = requests.post(url)
    
        
        time.sleep(1)

        try:
            print(osutrack)
            print(osutrack.text)
        except Exception as e:
            print (e)
            input()
            pass  
        
        if userid == userIDList[-1]:
            break
    
except Exception as e:
    print("Failed")
    print (e)
    input()
    pass
