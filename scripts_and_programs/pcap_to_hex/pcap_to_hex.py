import json
import subprocess as sp

def get_tshark_hexstreams(capture_path: str) -> list:
    cmds = ["tshark", "-x", "-r", capture_path, "-T", "json"]
    frames_text = sp.check_output(cmds, text=True)
    frames_json = json.loads(frames_text)
    hexstreams = [frame["_source"]["layers"]["frame_raw"][0] for frame in frames_json]
    return hexstreams

print("Enter path to input pcap file:")
input_filepath = input()

print("Enter path to output file:")
output_filepath = input()

output = get_tshark_hexstreams(input_filepath)

with open (output_filepath, "w") as file:
    for item in output:
        file.writelines(item)
        file.writelines("\n")
