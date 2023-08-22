import os
prefix = "prefix_"
suffix = "_suffix.py"
os.rename(__file__, __file__[::-1].replace(__file__.split("/")[-1].split("\\")[-1][::-1], "", 1)[::-1] + prefix + str(int(__file__.split("/")[-1].split("\\")[-1].replace(prefix, "", 1)[::-1].replace(suffix[::-1], "", 1)[::-1]) + 1) + suffix)
