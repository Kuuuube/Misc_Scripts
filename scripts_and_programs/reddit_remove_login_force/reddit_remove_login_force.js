// ==UserScript==
// @name         Reddit Remove Login Force
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/reddit_remove_login_force
// @version      0.3
// @description  Remove client side nsfw login forcing from reddit.
// @author       Kuuube
// @match        https://www.reddit.com/*
// @icon         https://www.google.com/s2/favicons?sz=64&domain=reddit.com
// ==/UserScript==

function remove_login_force() {
    // Desktop
    document.querySelectorAll(".configured-xpromo-modal").forEach(x => x.remove());

    let containers = document.querySelectorAll("xpromo-nsfw-blocking-container");
    for (const container of containers) {
        try {
            container.shadowRoot.querySelector(".prompt").remove();
        } catch {}
    }

    document.querySelector("body").classList.remove("rpl-scroll-lock");

    // Mobile
    document.querySelectorAll(".configured-xpromo-bottom-sheet").forEach(x => x.remove());
}

(function() {
    remove_login_force();
    const observer = new MutationObserver(function() {remove_login_force()});
    observer.observe((document.documentElement), {childList: true, subtree: true});
})();