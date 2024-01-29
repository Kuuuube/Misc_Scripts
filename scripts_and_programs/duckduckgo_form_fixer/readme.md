# Duckduckgo Form Fixer

Forces searching from the URL instead of POST when using html.duckduckgo. This allows proper use of the browser back button.

## Usage

- Install the userscript. This can be done in many ways, below are two ways to install through TamperMonkey. Other extensions/add-ons may require different methods.

    Create a new script and copy-paste the contents of `duckduckgo_form_fixer.js` into it.

    Open the `Dashboard`, navigate to the `Installed Userscripts` tab, and drag `duckduckgo_form_fixer.js` into it.

## Dependencies

Any userscript extension/add-on. I use [TamperMonkey](https://www.tampermonkey.net/).

## Notes

- To properly add html.duckduckgo to Firefox follow these steps: [How to add HTML DuckDuckGo in Firefox](../../notes/firefox_how_to_add_html_ddg.md). Adding it by right clicking the search bar when on the URL will cause issues.
