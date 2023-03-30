// ==UserScript==
// @name         Youtube Shorts Desktop Redirect
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/youtube_shorts_desktop_redirect
// @version      0.3
// @description  Redirects youtube shorts to the desktop player.
// @author       Kuuube
// @match        *://www.youtube.com/*
// @icon         https://www.google.com/s2/favicons?sz=64&domain=youtube.com
// ==/UserScript==

function redirect() {
    var url = window.location.pathname;
    if (url.includes("shorts/")) {
        window.stop();
        window.location.replace(url.replace("shorts/", "watch?v="));
    }
}

var replace_check = true;

function replace() {
    var links = document.getElementsByTagName("a");

    for (var i = 0; i < links.length; i++) {
        links[i].href = links[i].href.replace("shorts/", "watch?v=");
    }

    replace_check = true;
}

function debounce(method, delay) {
    if (replace_check) {
        method.debounce_timer = setTimeout(function() {method()}, delay);
        replace_check = false;
    }
}

(function() {
    redirect();
    window.addEventListener("yt-navigate-start", function() {redirect()});

    replace();
    window.addEventListener("DOMSubtreeModified", function() {debounce(replace, 100)});
    window.addEventListener("yt-navigate-finish", function() {replace()});
})();