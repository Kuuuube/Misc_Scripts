// ==UserScript==
// @name         Youtube Shorts Remover
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/youtube_shorts_remover
// @version      0.1
// @description  Removes youtube shorts.
// @author       Kuuube
// @run-at       document-start
// @match        *://www.youtube.com/*
// @icon         https://www.google.com/s2/favicons?sz=64&domain=youtube.com
// ==/UserScript==

function remove() {
    for (const shorts_section_renderer of document.querySelectorAll("ytd-rich-section-renderer")) {
        shorts_section_renderer.hidden = true;
    }
}

(function() {
    const observer = new MutationObserver(function() {remove()});
    observer.observe((document.documentElement), {childList: true, subtree: true});
})();
