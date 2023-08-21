# Self Renaming Script

A python script that renames itself iteratively each time it is run.

## Usage

- Edit the `prefix` and `suffix` variables to the prefix and suffix of your choosing.

- Rename `prefix_1_suffix.py` to match your prefix and suffix, including a number between the prefix and suffix.

- Run the script whenever you want it to iterate up its filename.

## Dependencies

Python 3: [Download link](https://www.python.org/downloads/)

## Notes

- This script is best used when you need to manually track an iteratively increasing number with a filename but it's inconvenient to have any additional config files present alongside the script.

- I use this for easy anime episode tracking. With the script named `_ep1.py` it can iterate up each time it is run (`_ep1.py` -> `_ep2.py` -> `_ep3.py` -> `...`).
