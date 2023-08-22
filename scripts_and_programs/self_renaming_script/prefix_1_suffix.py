import os
prefix = "prefix_"
suffix = "_suffix.py"
os.rename(__file__, __file__.split(prefix)[0] + prefix + str(int(__file__.split(prefix)[1].split(suffix)[0]) + 1) + suffix)
