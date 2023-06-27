# Word List Compressor

Scripts to compress a word list into the smallest amount of words by removing duplicate trigrams. (Original script written by [ClemenPine](https://github.com/ClemenPine/word-compressor))

Which version should you use?

- Rust: Runs fast, small executable

- Haskell: Runs fast, haskell is cool

- Python: You want to use the worst one

## Rust

### Usage

- Run `word-compressor.exe` and follow the prompts

    Json files with word lists under an array named `"words"` or `"texts"` are accepted. Monkeytype's `english_5k.json` has been included as an example.

    `Ignore uppercase and lowercase` treats trigrams with uppercase and lowercase the same. This does not change the case of words in the output.

    `Size of ngram to calculate` allows for changing the size of character slices taken from each word. The minimum ngram size is 1.

### Notes

- Estimated calculation times on english wordlists using ngram size 3:

    | Wordlist          | Time   |
    |-------------------|--------|
    | English 200       | 0.004s |
    | English 1,000     | 0.008s |
    | English 5,000     | 0.026s |
    | English 10,000    | 0.054s |
    | English 25,000    | 0.135s |
    | English 450,000   | 2.620s |
    | English 1,000,000 | 5.765s |

## Go

### Usage

- Install Go

- Run `go run .` and follow the prompts

### Notes

- Currently Go binaries have not been added. They may be added in the future. If you cannot compile/run it yourself I suggest using the Rust or Haskell versions instead.

## Dart

### Usage

- Install Dart

- Run `dart run` and follow the prompts

## Haskell

### Usage

- Extract `word-compressor.7z` (7-zip Archive), run `word-compressor.exe` with or without args

    ```
    word-compressor {input_file} {ignore_uppercase_and_lowercase (y/n)}
    ```

    Json files with word lists under an array named `"words"` or `"texts"` are accepted. Monkeytype's `english_5k.json` has been included as an example.

    `Ignore uppercase and lowercase` treats trigrams with uppercase and lowercase the same. This does not change the case of words in the output.

### Building

```
cabal build
```

### Notes

- The haskell version is *much* faster than the python version on large lists.

- Estimated calculation times on english wordlists:

    | Wordlist        | Time    |
    |-----------------|---------|
    | English 200     | 0.009s  |
    | English 1,000   | 0.017s  |
    | English 5,000   | 0.083s  |
    | English 10,000  | 0.190s  |
    | English 25,000  | 0.525s  |
    | English 450,000 | 10.750s |

## Python

### Usage

- Run `compress.py` and follow the prompts.

    Json files with word lists under an array named `"words"` or `"texts"` are accepted. Monkeytype's `english_5k.json` has been included as an example.

### Dependencies

Python 3: [Download link](https://www.python.org/downloads/)

### Notes

- The larger the word set is the more time it takes to remove a single word. This scales exponentially.

- Only json arrays named `"words"` or `"texts"` are accepted by default but new names can be added by adding another elif on line 11 of `compress.py`

    For example: Adding the following will search for an array named `something_else`:
    ```python
    elif 'something_else' in json_file:
    words = json_file['something_else']
    ```