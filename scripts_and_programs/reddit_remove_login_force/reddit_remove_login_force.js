// ==UserScript==
// @name         Reddit Remove Login Force
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/reddit_remove_login_force
// @version      0.1
// @description  Remove client side nsfw login forcing from reddit.
// @author       Kuuube
// @match        https://www.reddit.com/*
// @icon         https://www.google.com/s2/favicons?sz=64&domain=reddit.com
// ==/UserScript==

function remove_login_force() {
    // Desktop
    try {
        document.querySelector(".configured-xpromo-modal").remove();
    } catch {}
    try {
        let container = document.querySelector("xpromo-nsfw-blocking-container").shadowRoot;
        container.querySelector(".prompt").remove();
    } catch {}

    document.querySelector("body").classList.remove("rpl-scroll-lock");

    // Mobile
    try {
        document.querySelector(".configured-xpromo-bottom-sheet").remove();
    } catch {}
}

(function() {
    remove_login_force();
    const observer = new MutationObserver(function() {remove_login_force()});
    observer.observe((document.documentElement), {childList: true, subtree: true});
})();