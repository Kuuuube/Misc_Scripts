import sys
# add lib folder to path to allow accessing modules there
sys.path.append(sys.path[0] + "/lib")

import requests

print(requests.get("https://example.com"))
