# Number Generator

Generates a newline separated list of numbers with customizeable start, end, prefix, and suffix.

## Usage

- Run `numgen.exe` or `numgen.py` and follow the prompts.

numgen.exe/numgen.c only:

- `Set Input Length` is the length in characters that can be input into the other prompts. 

    When entering extremely long prefixes or suffixes you may need to use above 1000.

## Building

numgen.c:

Compiled using gcc 12.1.0:

```
gcc numgen.c -O2 -o numgen.exe
```

## Notes

Some C compilers will throw errors when compiling `numgen.c`. Use gcc 12.1.0 if you run into issues.

In Python 3.10.0, `numgen.py` ran nearly 100x slower than `numgen.c/numgen.exe` on my pc.