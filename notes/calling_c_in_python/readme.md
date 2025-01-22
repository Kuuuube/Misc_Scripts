# Calling C in Python

Simple example of calling C in Python. `multiply.py` calls `libmultiply.so` (compiled from `libmultiply.c`) to multiply each element in a list of floats by a multiplier.

## Compiling and Running

`libmultiply.c` needs to be compiled into a shared library. `libmultiply.so` on Linux.

Run either `runcmake.sh` or `rungcc.sh` to compile with either cmake or gcc and run the python script `multiply.py`.
