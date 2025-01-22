# KeePass DataBaseBackup Patch

Minor changes to the KeePass DataBaseBackup plugin.

## Features

- `Number backup to keep` may be set up to u32 max / 2 (2147483647).

## Usage

- Download the `dbBackup.plgx` file and place it in the keepass `Plugins` directory.

## Building

Build targetting the `dbBackup` directory. `dbBackup.plgx` will be created in the parent directory of `dbBackup`.

```
KeePass.exe --plgx-create dbBackup
```

## Notes

- The original version by Francis Noël can be found here: [databasebackup](https://keepass.info/plugins.html#databasebackup). This fork was built off of `DataBaseBackup-2.0.8.6`.

- The following note is provided with the original version: `Note: DataBaseBackup does not use KeePass' I/O infrastructure, therefore the plugin is incompatible with most other plugins that are providing support for more protocols (like IOProtocolExt).`
