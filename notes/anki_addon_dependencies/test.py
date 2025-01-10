import sys
import os
# add lib folder to path to allow accessing modules there
sys.path.append(os.path.dirname(__file__) + "/lib")

import requests

print(requests.get("https://example.com"))
