# Unicode Range Generator

Writes unicode characters within a list of unicode ranges to file.

## Usage

- Set the list of hex encoded unicode code point ranges to generate using with `unicode_ranges` in `unicode_range_generator.py`.

    I have included various ranges within the CJK block as an example of how to add unicode ranges.

- Optionally, edit `delimiter` in `unicode_range_generator.py` to change the delimiter.

    Using no delimiter may cause characters to merge together when writing.

## Dependencies

Python 3: [Download link](https://www.python.org/downloads/)

## Notes

- Unicode code points are typically hex encoded. They look like `U+8AAD` or `\u8AAD`. If you see a unicode code point that looks like `35501` it is likely an integer code point. You must convert it to hex to use it with this tool.