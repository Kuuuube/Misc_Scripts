# ClemenPine Word List Compressor

My fork of a small script written by [ClemenPine](https://github.com/ClemenPine) to compress a word list into the smallest amount of words by removing duplicate trigrams.

## Usage

- Run `compress.py` and follow the prompts.

    Json files with word lists under an array named `"words"` or `"texts"` are accepted. Monkeytype's `english_5k.json` has been included as an example.

    `List size to generate` controls at how many words the plaintext words list will be separated by a newline.

    `Write list numbers` controls whether or not to number the lists in the plaintext words list.

    `Treat capital and lowercase the same` allows for not treating trigrams with capital letters as different trigrams from those with lowercase letters.

## Dependencies

Python 3: [Download link](https://www.python.org/downloads/)

Python `typing` module: To install it, enter the following command in cmd or a terminal:

```
pip install typing
```

## Notes

- This is very slow I should rewrite it in something faster than python at some point. 

    Printing progress for every word also could cause it to be somewhat slower but due to how slow it is already it probably doesnt matter much.

- The larger the word set is the more time it takes to remove a single word. The script becomes unbearably slow on lists over around 10,000 words.

- Only json arrays named `"words"` or `"texts"` are accepted by default but new names can be added by adding another elif on line 64 of `compress.py`

    For example: Adding the following on line 64 will search for an array named `something_else`:
    ```python
    elif 'something_else' in json_file:
    words = json_file['something_else']
    ```

- [ClemenPine's](https://github.com/ClemenPine) original script along with some explanation is included in the `original_source` folder.