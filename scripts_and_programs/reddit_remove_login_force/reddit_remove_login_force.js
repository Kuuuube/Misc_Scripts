// ==UserScript==
// @name         Reddit Remove Login Force
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/reddit_remove_login_force
// @version      0.4
// @description  Remove client side nsfw login forcing from reddit.
// @author       Kuuube
// @match        https://www.reddit.com/*
// @icon         https://www.google.com/s2/favicons?sz=64&domain=reddit.com
// ==/UserScript==

let ignore_next_mutation = true;

function remove_login_force() {
    if (ignore_next_mutation) {
        ignore_next_mutation = false;
        return;
    }

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
    document.querySelectorAll(".configured-xpromo-bottom-sheet-v2").forEach(x => x.remove());

    // All
    let maybe_embed_loaders = document.querySelectorAll("shreddit-async-loader");
    for (const maybe_embed_loader of maybe_embed_loaders) {
        try {
            if (maybe_embed_loader.getAttribute("bundlename") == "embed") {
                let embed_container = maybe_embed_loader.querySelector("shreddit-embed");
                let embed_html = embed_container.getAttribute("html");

                ignore_next_mutation = true; // avoid infinite looping from the below edit
                embed_container.outerHTML = embed_html;
            }
        } catch {}
    }
}

(function() {
    remove_login_force();
    const observer = new MutationObserver(function() {remove_login_force()});
    observer.observe((document.documentElement), {childList: true, subtree: true});
})();