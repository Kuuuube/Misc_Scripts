# TSV to Freq Dict

Converts a simple tsv from corpora word frequency analysis into a rank-based yomitan frequency dictionary.

## Usage

- Format your tsv file into the following format: `Word\tOccurances`. Do not include a header row.

    Example format:

    | Word | Occurances |
    |------|------------|
    | の   | 123567     |

- Sort by the `Occurances` column from highest to lowest.

- Name your file `list.tsv` and put it next to `tsv_to_freq_dict.py`.

- Run `tsv_to_freq_dict.py` or `tsv_to_freq_dict_ranged.py`

    `tsv_to_freq_dict.py` handles occurance collisions in the frequency list by giving all same-occurance words the same single frequency value.

    For example: If "abc" and "cba" collide at 50 occurances and the script is at rank 30, both will be given rank 30. Then next word after these two then gets rank 31.

    `tsv_to_freq_dict_ranged.py` handles occurance collisions in the frequency list by giving all same-occurance words a range of frequency values for the amount of words that have collided.

    For example: If "abc" and "cba" collide at 50 occurances and the script is at rank 30, both will be given rank 30-31. Then next word after these two then gets rank 32.

- Edit `index.json` with the metadata you want. Do not change the `format` field.

- Zip `index.json` and `term_meta_bank_1.json`

- The dictionary is now ready to be imported into yomitan!

## Dependencies

Python 3: [Download link](https://www.python.org/downloads/)

## Notes

- This does not handle converting rank based tsv frequency lists. Only occurance based.

    To easily identify frequency list types:

    Occurance based frequency lists have high numbers for common words. Rank based frequency lists have low numbers for common words.