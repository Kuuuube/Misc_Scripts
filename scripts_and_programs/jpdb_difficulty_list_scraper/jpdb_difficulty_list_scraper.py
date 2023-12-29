import requests
import re
import time
import datetime

def write_csv(filename, listing_items, listings):
    utc_time = datetime.datetime.utcnow().strftime("%Y-%m-%d")
    with open(filename + "_" + utc_time + ".csv", "w", encoding = "UTF-8") as csv_file:
        csv_file.write("\"Title\"," + "\"" + "\",\"".join(listing_items) + "\"" + "\n")
        for listing in listings:
            csv_file.write(listing + "\n")

def scrape_list(difficulty_list, listing_items):
    next_page = "0"
    listings = []
    while True:
        print(next_page)
        response = requests.get("https://jpdb.io/" + difficulty_list + "-difficulty-list?offset=" + next_page)
        response.encoding = response.apparent_encoding #fix jpdb reporting the wrong encoding for the web novel difficulty list

        listings_regex = re.findall("(<h5 style(\s|.)*?</div>)", response.text)
        if not listings_regex:
            break

        for listing in listings_regex:
            new_listing = ""
            title_regex = re.search("(?<=<h5 style=\"max-width: 30rem;\">).*?(?=</h5>)", listing[0])
            if not title_regex:
                print("Could not find title in listing")
                continue
            new_listing += "\"" + title_regex[0] + "\"" + ","
            for listing_item in listing_items:
                new_listing_regex = re.search("(?<=" + re.escape(listing_item) + "</th><td>)(\d+(/|\.|%|)\d*)", listing[0])
                if not new_listing_regex:
                    print(title_regex[0] + " missing " + listing_item)
                    new_listing += ","
                    continue
                new_listing += "\"" + new_listing_regex[0] + "\"" + ","
            listings.append(new_listing)

        next_page_regex = re.findall("(?<=-difficulty-list\?offset=)\d+", response.text)
        if not next_page_regex:
            break

        if int(next_page_regex[-1]) < int(next_page):
            break

        next_page = next_page_regex[-1]
        time.sleep(1)

    write_csv(difficulty_list, listing_items, listings)


difficulty_lists = [("anime", ["Length (in words)","Unique words","Unique words (used once)","Unique words (used once %)","Unique kanji","Unique kanji (used once)","Unique kanji readings","Difficulty","MAL avg. rating","MAL rating count"]),
                    ("novel", ["Length (in words)","Unique words","Unique words (used once)","Unique words (used once %)","Unique kanji","Unique kanji (used once)","Unique kanji readings","Difficulty","Average sentence length","Characters","Volumes"]),
                    ("visual-novel", ["Length (in words)","Unique words","Unique words (used once)","Unique words (used once %)","Unique kanji","Unique kanji (used once)","Unique kanji readings","Difficulty","Average sentence length","Characters","VNDB avg. rating","VNDB rating count"]),
                    ("web-novel", ["Length (in words)","Unique words","Unique words (used once)","Unique words (used once %)","Unique kanji","Unique kanji (used once)","Unique kanji readings","Difficulty","Average sentence length","Characters"]),
                    ("live-action", ["Length (in words)","Unique words","Unique words (used once)","Unique words (used once %)","Unique kanji","Unique kanji (used once)","Unique kanji readings","Difficulty"])]

for difficulty_list in difficulty_lists:
    print(difficulty_list)
    scrape_list(difficulty_list[0], difficulty_list[1])

