// ==UserScript==
// @name         Youtube Shorts Desktop Redirect
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/youtube_shorts_desktop_redirect
// @version      0.8
// @description  Redirects youtube shorts to the desktop player.
// @author       Kuuube
// @run-at       document-start
// @match        *://www.youtube.com/*
// @icon         https://www.google.com/s2/favicons?sz=64&domain=youtube.com
// ==/UserScript==

function redirect() {
    var url = window.location.origin + window.location.pathname;
    if (url.includes("youtube.com/shorts/")) {
        window.stop();
        window.location.replace(url.replace("youtube.com/shorts/", "youtube.com/watch?v="));
    }
}

function replace() {
    console.log("hit");
    var links = document.getElementsByTagName("a");
    for (var i = 0; i < links.length; i++) {
        if (links[i].href.includes("youtube.com/shorts/")) {
            links[i].href = links[i].href.replace("youtube.com/shorts/", "youtube.com/watch?v=");
        }
    }
}

(function() {
    redirect();
    window.addEventListener("yt-navigate-start", function() {redirect()});

    window.addEventListener("DOMContentLoaded", function() {replace()});
    window.addEventListener("yt-navigate-finish", function() {replace()});

    const observer = new MutationObserver(function() {replace()});
    observer.observe((document.documentElement), {childList: true, subtree: true});
})();