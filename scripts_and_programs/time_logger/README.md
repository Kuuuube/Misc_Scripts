# Time Logger

Logs timer times to csv with configurable tags and ui.

## Usage

- Run `time_logger_gui.pyw`.

- Optionally, configure settings in `config.ini`.

- Press one of the top buttons to start the time with that button's tag.

- Press `Record` to write to the csv file and reset the time or `Discard` to reset the time without writing to file.

- Optionally, run `graph.py` after recording some data to show a graph showing the sum of times recorded each day.

## Dependencies

Python 3: [Download link](https://www.python.org/downloads/)

## Notes

- The csv file uses the following format:

    ```
    Date,Duration,Tag
    ```

    Date is the datetime when the `Record` button was pressed.

    Duration is the timer duration when the `Record` button was pressed.

    Tag is the button name pressed when starting the timer.

- When importing the csv into applications such as libreoffice make sure to enable `Detect special numbers` (Excel does this by default). The durations may not be parsed correctly otherwise.