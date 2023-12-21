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

- Run `tsv_to_freq_dict.py`

## Dependencies

Python 3: [Download link](https://www.python.org/downloads/)

## Notes

- This does not handle converting rank based tsv frequency lists. Only occurance based.

    To easily identify frequency list types:

    Occurance based frequency lists have high numbers for common words. Rank based frequency lists have low numbers for common words.