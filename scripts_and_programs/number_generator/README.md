# Number Generator

Generates a newline separated list of numbers with customizeable start, end, prefix, and suffix.

## C

### Usage

- Run `numgen.exe` and follow the prompts.

- `Set Input Length` is the length in characters that can be input into the other prompts. 

    When entering extremely long prefixes or suffixes you may need to use above 1000.

numgen_block_dumping_test.exe only:

- `Max memory usage (kb)` sets the max memory to use before dumping to file.

    Entering `0` will cause it to write all data to memory before dumping.

    Entering `1` will cause it to write line by line exactly like `numgen.c/numgen.exe` does.

    Entering anything other number will define an amount of memory it will dump to the file before looping again. If the number defined is less than the amount of memory required for two lines it will instead write all data to memory before dumping.

### Building

Compiled using gcc 12.1.0:

```
gcc numgen.c -O2 -o numgen.exe
```

### Notes

- Some C compilers will throw errors when compiling `numgen.c` or `numgen_block_dumping_test.c`. Use gcc 12.1.0 if you run into issues.

- `numgen_block_dumping_test.c` isn't actually faster than `numgen.c` in all cases I tested (When `Max memory usage (kb)` is not set to 0 of course).

<br>

## Rust

### Usage

- Run `numgen.exe` and follow the prompts.

### Building

```
cargo build --release
```

<br>

## Python

### Usage

- Run `numgen.py` and follow the prompts.

## Dependencies

- Python 3: [Download link](https://www.python.org/downloads/)

<br>

## Fortran

### Usage

- Run `numgen.exe` and follow the prompts.

    Enter "" to use an empty prefix or suffix. The prompt will not let you continue by only sending a newline.

### Building

Compiled using gcc 12.1.0:

```
gfortran -O2 numgen.f -o numgen.exe
```

### Notes

- Has F77 syntax but (I think) uses some non-F77 features.