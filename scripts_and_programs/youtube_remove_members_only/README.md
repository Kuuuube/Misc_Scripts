# Youtube Remove Members Only

Removes members only videos from channel video lists.

## Usage

- Install the userscript. This can be done in many ways, below are two ways to install through TamperMonkey. Other extensions/add-ons may require different methods.

    Create a new script and copy-paste the contents of `youtube_remove_members_only.js` into it.

    Open the `Dashboard`, navigate to the `Installed Userscripts` tab, and drag `youtube_remove_members_only.js` into it.

## Dependencies

Any userscript extension/add-on. I use [TamperMonkey](https://www.tampermonkey.net/).

## Notes

- If you are a member of certain channels and would like to still see their member only videos, you can add an exclude.

    You can exclude one url per line. Place the exclude on a new line inside the top block between `// ==UserScript==` and `// ==/UserScript==`.

    An exclude looks like this:

    ```
    // @exclude    *://www.youtube.com/@kuuube*
    ```
