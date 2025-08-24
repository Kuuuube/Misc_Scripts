// ==UserScript==
// @name         FPC Get Swatch Color
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/fpc_get_swatch_color
// @version      0.1
// @description  On FPC allows viewing any ink swatch color by hovering over it or copying it by clicking on it.
// @author       Kuuuube
// @match        https://www.fountainpencompanion.com/*
// @icon         https://www.google.com/s2/favicons?sz=64&domain=fountainpencompanion.com
// ==/UserScript==

(function() {
    'use strict';

    async function set_clipboard(text) {
        const type = "text/plain";
        const clipboard_item_data = {
            [type]: text,
        };
        const clipboard_item = new ClipboardItem(clipboard_item_data);
        await navigator.clipboard.write([clipboard_item]);
    }

    function get_hex_background_color(target_element) {
        const css_value = target_element.attributes.style.value;
        const background_color_regex = css_value.match("(?<=background-color:\s*#)[a-zA-Z0-9]{6}");
        if (background_color_regex) {
            return "#" + background_color_regex;
        }
        return null;
    }

    function get_rgb_background_color(target_element) {
        const stylesheet = new CSSStyleSheet();
        const css = "body {" + target_element.attributes.style.value + "}";
        stylesheet.replaceSync(css);
        for (const cssRule of stylesheet.cssRules) {
            if (!(cssRule instanceof CSSStyleRule)) { continue; }
            if (cssRule.style["background-color"]) {
                return cssRule.style["background-color"];
            }
        }
    }

    for (const target_element of document.querySelectorAll("*")) {
        if (target_element.attributes?.style?.value.includes("background-color")) {
            const hex_background_color = get_hex_background_color(target_element);
            let background_color = "";
            if (hex_background_color && hex_background_color.match("#[a-zA-Z0-9]{6}")) {
                background_color = hex_background_color;
            } else {
                background_color = get_rgb_background_color(target_element);
            }
            if (!background_color) { continue; }

            target_element.title = background_color + " (Click to copy)";
            target_element.style.cursor = "pointer";

            target_element.addEventListener("click", () => {
                set_clipboard(background_color);
            });
        }
    }
})();