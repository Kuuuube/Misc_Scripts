#!/usr/bin/env python3

import webbrowser
import sys
import re
import subprocess

url_to_open = re.search("(?<=URL\=).*", open(sys.argv[1], "r").read())[0]

subprocess.Popen("xdg-open " + url_to_open, shell=True)

#webbrowser often fails to open the default browser correctly
#webbrowser.open(url_to_open)
