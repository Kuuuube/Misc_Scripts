# osu! Collection Factory Tools

## Autodumper osu! Collector

Dumps osu! Collector collection data to OSDB recursively starting at the id in `start_number.txt` and ending at the latest id.

### Usage

- Edit the first line of `start_number.txt` to the collection id to start on

- Run `launcher_auto.py`

### Notes

- The OSDB files generated from this do not contain all data. They contain MapsetID, MapID, and MD5. This is enough data do download the maps and import the collections through [Collection Manager](https://github.com/Piotrekol/CollectionManager) if you already have the maps downloaded but not enough data do fully identify the maps in [Collection Manager](https://github.com/Piotrekol/CollectionManager) without them downloaded.

- osu! Collector does store all map data to fill full osdb parameters. However, it can only grab 100 maps in a collection per request making it much slower. I plan on adding support for this in the future.

- The CSV to OSDB conversion is handled by [osu! Collection Converter](https://github.com/Kuuuube/osu_CollectionConverter).

- There is a delay of 5 seconds between each collection request. This is much longer than required and if you are dumping large amounts of collections you may want to change it to 1 second. The delay can be found on the last line of `osu_collector_dump` in `osu_collector_dumper.py`.