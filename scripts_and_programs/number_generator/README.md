# Number Generator

Generates a newline separated list of numbers with customizeable start, end, prefix, and suffix.

## Rust

### Usage

- Run `numgen` and follow the prompts.

### Building

```
cargo build --release
```

<br>

## C

### Usage

- Run `numgen` and follow the prompts.

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
gcc numgen.c -O2 -o numgen
```

### Notes

- Some C compilers will throw errors when compiling `numgen.c` or `numgen_block_dumping_test.c`. Use gcc 12.1.0 if you run into issues.

- `numgen_block_dumping_test.c` isn't actually faster than `numgen.c` in all cases I tested (When `Max memory usage (kb)` is not set to 1 of course).

<br>

## Haskell

### Usage

- Run `numgen`, and follow the prompts.

### Building

```
ghc numgen.hs -O2
strip numgen.exe
```

<br>

## Cobol

### Usage

1. Run `numgen` and follow the prompts.

### Building

Compiled using [GnuCobol](https://gnucobol.sourceforge.io/) 3.2.0

```
cobc -x numgen.cob -O2 -o numgen
```

### Notes

- Prefix and suffix have all leading and trailing whitespace stripped.

- The length of prefix or suffix cannot exceed 500 characters.

- The full length of each line written to file cannot exceed 1000 characters. Any characters above this limit will be cut off.

<br>

## Python

### Usage

- Run `numgen.py` and follow the prompts.

## Dependencies

- Python 3: [Download link](https://www.python.org/downloads/)

<br>

## Fortran

### Usage

- Run `numgen` and follow the prompts.

    Enter "" to use an empty prefix or suffix. The prompt will not let you continue by only sending a newline.

### Building

Compiled using gcc 12.1.0:

```
gfortran -O2 numgen.f -o numgen
```

### Notes

- Has F77 syntax but (I think) uses some non-F77 features.

<br>

## Zig

### Usage

- Run `numgen` and follow the prompts.

### Building

Compiled using zig 0.12.1:

```
zig build --release=fast
```

<br>

## B

### Usage

- Run `numgen` with args

    ```
    numgen {start number} {end number} {prefix} {suffix}
    ```

### Building

Compiled using [B compiler](https://github.com/aap/b) 2e53fbf and assembled using gcc 9.4.0:

```
bcompile numgen.b numgen
```

### Notes

- I have included a build of the compiler, compile script, and built binary as this can be difficult to get set up and working.

- This compiler does not support windows and all binaries included target Linux-x64. Other B compilers do exist that support windows but this code may not compile or function elsewhere.

<br>

## Modula-2

### Usage

- Run `numgen` and follow the prompts.

### Building

Compiled using gm2 14.1.1:

```
gm2 numgen.mod -O2 -o numgen
```

Notes:

- References for Modula-2: [gm2 docs](https://gcc.gnu.org/onlinedocs/gm2/), [modula-2 docs](https://www.modula2.org/reference/index.php)

<br>

## Speed Rankings

These are NOT objective or fair rankings for the speed of these languages, only how fast my code runs. My code is not perfectly optimized. Do not take these results as any representation of the speed of a language.

Each numgen is tested with `Start number: 0`, `End number: 10000000`, `Prefix: test`, `Suffix: test`.

| Language      | Runtime       |
| ------------- | ------------- |
| Rust          | 813ms         |
| C             | 891ms         |
| Haskell       | 3.5s          |
| Python        | 5.7s          |
| Cobol         | 10.4s         |
| Fortran       | 15.2s         |
| Zig           | 21.0s         |
| B             | 3.3m          |
| Modula-2      | 4.4m          |
