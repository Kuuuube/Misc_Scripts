# Pcap to Hex

Script to write a hex dump from pcap data for every packet using tshark.

## Usage

- Run `pcap_to_hex.py`

- Enter the input and output paths

## Dependencies

Wireshark: [Download Link](https://www.wireshark.org/#download)

## Notes

- Tshark is a utility that comes with wireshark. 

    This script requires tshark to be on a PATH variable. If this isn't set up be default there are plenty of guides on how to add a folder to the PATH variables.

    Alternatively, you can change the path in the script to point to the folder where tshark is located. (It is best to use `/` instead of `\` or `\\` between directories even on Windows.)