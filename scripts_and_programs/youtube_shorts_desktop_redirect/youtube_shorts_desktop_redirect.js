// ==UserScript==
// @name         Youtube Shorts Desktop Redirect
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/youtube_shorts_desktop_redirect
// @version      0.1
// @description  Redirects youtube shorts to the desktop player.
// @author       Kuuube
// @match        *://www.youtube.com/*
// @icon         https://www.google.com/s2/favicons?sz=64&domain=youtube.com
// ==/UserScript==

function redirect() {
    var url = window.location.pathname;
    if (url.includes("shorts/")) {
        window.location.replace(url.replace("shorts/", "watch?v="));
    }
}

(function() {
    redirect();
    window.addEventListener('yt-navigate-finish', function() {redirect()})
})();