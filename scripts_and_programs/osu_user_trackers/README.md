# osu! User Trackers

Tools for tracking osu! data from a list of users.

## Usage

- Add your api key at line 18 of `osu_user_tracker.py` and line 16 of `osuskills_tracker.py` where it says `k = ""`. Your osu! api v1 key should be put between the quotes.

- Add your api key at lines 9 and 10 of `osu_user_tracker_api_v2.py` where it says `client_id = ''` and `client_secret = ''`. Your osu! api v2 key should be put between the quotes.

- Enter a newline separated list of userids into `list.txt`.

- Run all desired scripts. 

    `osu_user_tracker.py` will retrieve official api v1 data for all users in the list and save them in individual files.

    `osu_user_tracker_api_v2.py` will retrieve official api v2 data for all users in the list and save them in individual files.

    `osuskills_tracker.py` will retrieve [osu!Skills](https://osuskills.com/) data and trigger an [osu!Skills](https://osuskills.com/) update for all users in the list and save them in individual files. (It saves somewhat broken HTML grabs but all useful data is intact. You may need to create your own solution to parse the data out.)

    `osutrack_ameobea_api_auto_updater.py` will trigger an [osu!track](https://ameobea.me/osutrack/) update for all users in the list. Do not use this script to mass update users. [Ameobea](https://github.com/Ameobea) regarding api usage: "The one thing I request is that you do not use the API to scrape the full osu!track database or mass-update users."

## Dependencies

Python `requests` module: To install it, enter the following command in cmd or a terminal:

```
pip install requests
```

## Notes

- All of these scripts have a 1 second delay between requests. I advise against changing this but if you must, you can search for `time.sleep(1)` in the files.

- Every script besides `osutrack_ameobea_api_auto_updater.py` have error checking. They will pause and print the error if any error is caught. If you need to disable this, remove lines that include `input()`.

- `osutrack_ameobea_api_auto_updater.py` does not have specific error checking beyond checking if the request fails. This is due to it not saving any files and all information contained in [osu!track](https://ameobea.me/osutrack/) being available in the official api from `osu_user_tracker.py` and `osu_user_tracker_api_v2.py`.

- `osu_user_tracker_api_v2.py` requests a token only once on startup and does not refresh the token for the duration of the runtime. An api v2 token lasts 24 hours at most. I advise against using these scripts like this but if you for some reason have the script requesting so many users that it runs over 24 hours it will break.

- The reason `osuskills_tracker.py` requires an api key despite [osu!Skills](https://osuskills.com/) not requiring one to update is due to [osu!Skills](https://osuskills.com/) using usernames instead of userids. The script requests data on the userid through the official api to get the username. This allows `osuskills_tracker.py` to use the same id list as the rest of the scripts.