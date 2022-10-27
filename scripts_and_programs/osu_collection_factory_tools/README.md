# osu! Collection Factory Tools

Tools for handling and generating large collections and large amounts of collections.

## Autodumper osu! Collector

Dumps osu! Collector collection data to OSDB recursively starting at the id in `start_number.txt` and ending at the latest id.

### Usage

- Edit the first line of `start_number.txt` to the collection id to start on (the start number will be automatically updated to the most recent id every time the program runs)

- Run `launcher_auto.py` for faster dumping but less data. Each collection requires only one request for the maps and metadata combined. 

    The generated OSDB will contain MapsetID, MapID, and MD5. This is enough data do download the maps and import the collections through [Collection Manager](https://github.com/Piotrekol/CollectionManager) if you already have the maps downloaded but not enough data do fully identify the maps in [Collection Manager](https://github.com/Piotrekol/CollectionManager) without them downloaded.

OR

- Run `launcher_auto_v2_endpoint.py` for slower dumping but full data. Each collection requires one request for every 100 maps in a collection and one request for metadata.

    The generated OSDB will contain full data. This is enough to fully identify every map in every collection.



### Notes

- The CSV to OSDB conversion is handled by [osu! Collection Converter](https://github.com/Kuuuube/osu_CollectionConverter).

- There is a delay of 5 seconds between each collection request. This is much longer than required and if you are dumping large amounts of collections you may want to change it to 1 second. The delay can be found on the last line of `osu_collector_dump` or `osu_collector_dump_v2_endpoint` in `osu_collector_dumper.py`.