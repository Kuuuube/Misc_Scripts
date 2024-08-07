#!/usr/bin/env python

import time
import json
import re
import requests
import sys

from argparse import ArgumentParser
from base64 import urlsafe_b64encode
from hashlib import sha256
from pprint import pprint
from secrets import token_urlsafe
from urllib.parse import urlencode
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options as ChromeOptions


# Latest app version can be found using GET /v1/application-info/android
USER_AGENT = "PixivAndroidApp/5.0.234 (Android 9.0; Pixel 3)"
REDIRECT_URI = "https://app-api.pixiv.net/web/v1/users/auth/pixiv/callback"
LOGIN_URL = "https://app-api.pixiv.net/web/v1/login"
AUTH_TOKEN_URL = "https://oauth.secure.pixiv.net/auth/token"
CLIENT_ID = "MOBrBDS8blbauoSck0ZfDbtuzpyT"
CLIENT_SECRET = "lsACyCD94FhDUtGTXi3QzcFE2uU1hqtDaKeqrdwj"
REQUESTS_KWARGS = {
    # 'proxies': {
    #     'https': 'http://127.0.0.1:1087',
    # },
    # 'verify': False
}

def s256(data):
    """S256 transformation method."""

    return urlsafe_b64encode(sha256(data).digest()).rstrip(b"=").decode("ascii")


def oauth_pkce(transform):
    """Proof Key for Code Exchange by OAuth Public Clients (RFC7636)."""

    code_verifier = token_urlsafe(32)
    code_challenge = transform(code_verifier.encode("ascii"))

    return code_verifier, code_challenge


def print_auth_token_response(response):
    data = response.json()

    try:
        access_token = data["access_token"]
        refresh_token = data["refresh_token"]
    except KeyError:
        print("error:")
        pprint(data)
        return

    print("access_token:", access_token)
    print("refresh_token:", refresh_token)
    print("expires_in:", data.get("expires_in", 0))


def login():
    options = ChromeOptions()
    options.set_capability("goog:loggingPrefs", {"performance": "ALL"})  # enable performance logs

    chromedriver_service = Service(executable_path="./chromedriver")
    driver = webdriver.Chrome(service=chromedriver_service, options=options)

    code_verifier, code_challenge = oauth_pkce(s256)
    login_params = {
        "code_challenge": code_challenge,
        "code_challenge_method": "S256",
        "client": "pixiv-android",
    }
    print("[INFO] Gen code_verifier:", code_verifier)

    driver.get(f"{LOGIN_URL}?{urlencode(login_params)}")

    while True:
        # wait for login
        if driver.current_url[:40] == "https://accounts.pixiv.net/post-redirect":
            break
        time.sleep(1)

    # filter code url from performance logs
    code = None
    for row in driver.get_log('performance'):
        data = json.loads(row.get("message", {}))
        message = data.get("message", {})
        if message.get("method") == "Network.requestWillBeSent":
            url = message.get("params", {}).get("documentURL")
            if url[:8] == "pixiv://":
                code = re.search(r'code=([^&]*)', url).groups()[0]
                break

    driver.close()

    print("[INFO] Get code:", code)

    response = requests.post(
        AUTH_TOKEN_URL,
        data={
            "client_id": CLIENT_ID,
            "client_secret": CLIENT_SECRET,
            "code": code,
            "code_verifier": code_verifier,
            "grant_type": "authorization_code",
            "include_policy": "true",
            "redirect_uri": REDIRECT_URI,
        },
        headers={
            "user-agent": USER_AGENT,
            "app-os-version": "9.0",
            "app-version": "5.0.234",
            "app-os": "android",
        },
        **REQUESTS_KWARGS
    )

    print_auth_token_response(response)


def refresh(refresh_token):
    response = requests.post(
        AUTH_TOKEN_URL,
        data={
            "client_id": CLIENT_ID,
            "client_secret": CLIENT_SECRET,
            "grant_type": "refresh_token",
            "include_policy": "true",
            "refresh_token": refresh_token,
        },
        headers={
            "user-agent": USER_AGENT,
            "app-os-version": "14.6",
            "app-os": "ios",
        },
        **REQUESTS_KWARGS
    )
    print_auth_token_response(response)

def main():
    if len(sys.argv) > 1:
        match sys.argv[1]:
            case "login":
                login()
            case "refresh":
                refresh(sys.argv[2])
            case "-h" | "--help":
                print("Pixiv Auth Grabber\nUsage: pixiv_auth_grabber.py [OPTION]...\n\nOptional:\n  login                          Log in to grab Access token and Refresh token (default when no args passed)\n  refresh OLD_REFRESH_TOKEN      Refresh an old Refresh token\n  -h,--help                      Display this help message\n")
            case _:
                print("Pixiv Auth Grabber: unknown option\nUsage: pixiv_auth_grabber.py [OPTION]...\n\nTry `pixiv_auth_grabber.py --help` for more options.\n")
    else:
        login()

    input("Finished. Press any key to exit...")

if __name__ == "__main__":
    main()
