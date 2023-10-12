# Time Logger

Logs timer times to csv with configurable tags and ui.

## Usage

- Optionally, configure settings in `config.ini`.

- Run `time_logger_gui.pyw`.

- Press one of the top buttons to start the time with that button's tag.

- Press `Record` to write to the csv file and reset the time or `Discard` to reset the time without writing to file.

## Dependencies

Python 3: [Download link](https://www.python.org/downloads/)

## Notes

- The csv file uses the following format:

    ```
    Date,Duration,Tag
    ```

    Date is the datetime the `Record` button was pressed.

    Duration is the timer duration before pressing `Record`.

    Tag is the button name pressed when starting the timer.

- When importing the csv into applications such as libreoffice make sure to enable `Detect special numbers` (Excel does this by default). The durations may not be parsed correctly otherwise.
