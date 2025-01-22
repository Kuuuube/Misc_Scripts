import ctypes

input_list = [1,2,3,4,5]
multiplier = 10
output = (ctypes.c_float * len(input_list))()

shared_lib = ctypes.CDLL("./libmultiply.so")
shared_lib.multiply.restype = None # returns void
shared_lib.multiply.argstype = [ctypes.POINTER(ctypes.c_float), ctypes.POINTER(ctypes.c_float), ctypes.c_uint, ctypes.c_int]

shared_lib.multiply((ctypes.c_float * len(input_list))(*input_list), output, len(input_list), multiplier)

print(list(output))
