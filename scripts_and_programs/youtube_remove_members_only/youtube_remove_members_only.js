// ==UserScript==
// @name         Youtube Remove Members Only
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/youtube_remove_members_only
// @version      0.1
// @description  Removes members only videos from channel video lists.
// @author       Kuuube
// @match        *://www.youtube.com/*
// @icon         https://www.google.com/s2/favicons?sz=64&domain=youtube.com
// ==/UserScript==

function remove_members_only() {
    let video_containers = document.querySelectorAll("ytd-rich-item-renderer.ytd-rich-grid-renderer")
    for (let i = video_containers.length - 1; i >= 0; i--) {
        if (video_containers[i].querySelector(".badge-style-type-members-only")) {
            video_containers[i].remove();
        }
    }
}

(function() {
    remove_members_only();
    const observer = new MutationObserver(function() {remove_members_only()});
    observer.observe((document.documentElement), {childList: true, subtree: true});
})();