# OpenTabletDriver Tablet Data Analysis

Reads OTD tablet data recordings and does some analysis.

## Usage

- Run `tablet_data_analysis.py {data_capture_filename} {filter}`

    `{filter}` is optional.

    Examples:
    ```
    tablet_data_analysis.py PTK-470_capture_otd_0.txt IntuosV3ExtendedReport
    tablet_data_analysis.py PTK-470_capture_otd_0.txt
    ```

## Dependencies

Python 3: [Download link](https://www.python.org/downloads/)

## Notes

- Currently only supports analysis for pressure timings.
