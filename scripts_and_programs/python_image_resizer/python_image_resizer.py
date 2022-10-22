#!/usr/bin/python
from PIL import Image
import os, sys

Image.MAX_IMAGE_PIXELS = 99999999999999

import warnings
warnings.filterwarnings("ignore")

path = "resize/"
dirs = os.listdir( path )

def resize():
    for item in dirs:
        if os.path.isfile(path+item):
            im = Image.open(path+item)
            f, e = os.path.splitext(path+item)
            imResize = im.resize((1920,834), Image.ANTIALIAS)
            imResize.save(f + '.png')

resize()
