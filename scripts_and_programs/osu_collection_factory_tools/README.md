# osu! Collection Factory Tools

Tools for handling and generating large collections and large amounts of collections.

## Autodumper osu! Collector

Dumps [osu! Collector](https://osucollector.com/) collection data to OSDB recursively starting at the id in `start_number.txt` and ending at the latest id.

### Usage

- Edit the first line of `start_number.txt` to the collection id to start on (the start number will be automatically updated to the most recent id every time the program runs)

- Run `launcher_auto.py` for faster dumping but less data. Each collection requires only one request for the maps and metadata combined. 

    The generated OSDB will contain MapsetID, MapID, and MD5. This is enough data to download the maps and import the collections through [Collection Manager](https://github.com/Piotrekol/CollectionManager) if you already have the maps downloaded but not enough data to fully identify the maps in [Collection Manager](https://github.com/Piotrekol/CollectionManager) without them downloaded.

OR

- Run `launcher_auto_v2_endpoint.py` for slower dumping but full data. Each collection requires one request for every 100 maps in a collection and one request for metadata.

    The generated OSDB will contain full data. This is enough to fully identify every map in every collection.

### Dependencies

Python `requests` module: To install it, enter the following command in cmd or a terminal:

```
pip install requests
```

.Net Runtime 6.0 x64: https://dotnet.microsoft.com/en-us/download/dotnet/6.0

### Notes

- The CSV to OSDB conversion is handled by [osu! Collection Converter](https://github.com/Kuuuube/osu_CollectionConverter).

- There is a delay of 5 seconds between each collection request. This is much longer than required and if you are dumping large amounts of collections you may want to change it to 1 second. The delay can be found on the last line of `osu_collector_dump` or `osu_collector_dump_v2_endpoint` in `osu_collector_dumper.py`.

## Autodumper Sheets

Creates collections based off a list of mapIDs or setIDs along with their collection names in `sheet_list.txt`.

### Usage

- Add your api key at the start of `launcher_id_then_name.py`, `launcher_name_then_id.py`, and `launcher_without_generation.py` where it says `api_key = ""`. Your osu! api v1 key should be put between the quotes.

- Add your list of collection names and mapIDs or setIDs to `sheet_list.txt`. The delimiter used between collection names and mapIDs or setIDs is `<||>`. All separate entries must be separated by a newline. Entries can be added in any order and collections of the same name do not need to be grouped together in `sheet_list.txt`.

    For example, to add mapID `123` to a collection named `Example Collection` you would input:

    ```
    123<||>Example Collection
    ```
    OR
    ```
    Example Collection<||>123
    ```

- Run either `launcher_id_then_name.py` or `launcher_name_then_id.py` depending on the order you have entered your collection name and mapIDs or setIDs.

### Dependencies

Python `requests` module: To install it, enter the following command in cmd or a terminal:

```
pip install requests
```

.Net Runtime 6.0 x64: https://dotnet.microsoft.com/en-us/download/dotnet/6.0

### Notes

- The CSV to OSDB conversion is handled by [osu! Collection Converter](https://github.com/Kuuuube/osu_CollectionConverter).

- There is a delay of 1 second between each osu! api request. This is the recommended speed to use as stated by peppy. I advise against changing this, but if you must, it is located in `list_mapid_info_puller.py` near the end of `id_to_db`.

- If the generation gets interrupted but some collections are already finished you should remove the files that match the names of already generated collections from `files_to_read` then run `launcher_without_generation.py` to finish the collection generation.

- Sometimes the osu! api will time out or you may get ratelimited in the middle of large collections or large amounts of collections being generated. Check `log.txt` to see if any of them failed. If collections did fail, remove all files from `files_to_read` except those matching names of failed collections then run `launcher_without_generation.py`.

- The code for this is somewhat spaghettified. If you need to make major changes to this program I suggest rewriting it instead.