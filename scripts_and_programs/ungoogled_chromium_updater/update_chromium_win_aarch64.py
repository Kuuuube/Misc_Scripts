import requests
import re
import subprocess
import os
import traceback

architecture = "arm64"

try:
    github_latest = requests.get("https://github.com/ungoogled-software/ungoogled-chromium-windows/releases/latest", allow_redirects=False)
    latest_version_tag = re.search("(?<=tag/).*$", github_latest.headers['Location'])[0]
    print("Updating to version: " + latest_version_tag)

    download_link = "https://github.com/ungoogled-software/ungoogled-chromium-windows/releases/download/" + latest_version_tag + "/ungoogled-chromium_" + latest_version_tag + "_installer_" + architecture + ".exe"
    print("Downloading: " + "ungoogled-chromium_" + latest_version_tag + "_installer_" + architecture + ".exe")

    installer_download = requests.get(download_link, stream=True)
    with open(latest_version_tag + "_installer_" + architecture + ".exe", "wb") as installer_file:
        for chunk in installer_download.iter_content(chunk_size=1024): 
            if chunk:
                installer_file.write(chunk)

    print("Running installer")
    subprocess.run(latest_version_tag + "_installer_" + architecture + ".exe", shell=True)

    print("Deleting installer")
    os.remove(latest_version_tag + "_installer_" + architecture + ".exe")

except Exception:
    print(traceback.format_exc())
    input()
