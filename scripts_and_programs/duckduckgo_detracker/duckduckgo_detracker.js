// ==UserScript==
// @name         Duckduckgo Detracker
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/duckduckgo_detracker
// @version      0.1
// @description  Removes tracker links from direct html.duckduckgo search results.
// @author       Kuuube
// @match        https://*.duckduckgo.com/*
// @icon         https://www.google.com/s2/favicons?sz=64&domain=duckduckgo.com
// ==/UserScript==

(function() {
    var links = document.getElementsByTagName("a");
    for (var i = 0; i < links.length; i++) {
        links[i].href = decodeURIComponent(links[i].href).replaceAll(/duckduckgo.com\/l\/\?uddg\=https:\/\//g, "").replaceAll(/&rut=.*/g, "")
    }
})();