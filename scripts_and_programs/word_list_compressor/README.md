# Word List Compressor

Scripts to compress a word list into the smallest amount of words by removing duplicate trigrams. (Original script written by [ClemenPine](https://github.com/ClemenPine))

## Haskell

### Usage

- Extract `word-compressor.7z` (7-zip Archive), run `word-compressor.exe`, and follow the prompts

    Json files with word lists under an array named `"words"` or `"texts"` are accepted. Monkeytype's `english_5k.json` has been included as an example.

    `Ignore uppercase and lowercase` treats trigrams with uppercase and lowercase the same. This does not change the case of words in the output.

    `Chunk size` defines the start size to chunk the list into.

    `Chunk multiplier` defines the size to multiply the chunk size by every pass. When the chunk size exceeds the list size one final pass is done with the entire list.

### Building

```
cabal build
```

### Notes

- The haskell version is *much* faster than the python version and I recommend using it. The python version only works decently on small lists.

-   Chunking can cause huge performance benefits on large lists. Below is an approximate comparison of the time to calculate word lists with default settings and recommended chunking settings. The lists used were taken from monkeytype.

    Lists with an extremely low amount of duplicate trigrams may see a loss in performance but generally it will help greatly.

    | Wordcount | No chunking (Default)    | Chunking | Chunk size/Chunk multiplier |
    |-----------|--------------------------|----------|-----------------------------|
    | 200       | 0.03s                    | 0.03s    | 100/2                       |
    | 1000      | 0.73s                    | 0.66s    | 500/2                       |
    | 5000      | 19.99s                   | 7.46s    | 500/2.5                     |
    | 10000     | 84.94s                   | 26.87s   | 1000/1.9                    |
    | 25000     | 586.80s                  | 126.32s  | 2500/1.5                    |
    | 450000    | 725760.00s*              | 2913.46s | 1000/5                      |

    ###### *The 450000 calculation with no chunking is a theoretical estimate due to the unreasonable amount of time it would take to run.

## Python

### Usage

- Run `compress.py` and follow the prompts.

    Json files with word lists under an array named `"words"` or `"texts"` are accepted. Monkeytype's `english_5k.json` has been included as an example.

    `List size to generate` controls at how many words the plaintext words list will be separated by a newline.

    `Write list numbers` controls whether or not to number the lists in the plaintext words list.

    `Treat capital and lowercase the same` allows for not treating trigrams with capital letters as different trigrams from those with lowercase letters.

### Dependencies

Python 3: [Download link](https://www.python.org/downloads/)

### Notes

- The larger the word set is the more time it takes to remove a single word. The script becomes unbearably slow on lists over around 10,000 words.

- Only json arrays named `"words"` or `"texts"` are accepted by default but new names can be added by adding another elif on line 64 of `compress.py`

    For example: Adding the following on line 64 will search for an array named `something_else`:
    ```python
    elif 'something_else' in json_file:
    words = json_file['something_else']
    ```

- [ClemenPine's](https://github.com/ClemenPine) original script along with some explanation is included in the `original_source` folder.