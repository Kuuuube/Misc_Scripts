// ==UserScript==
// @name         Jisho Default Scroll Fix
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/jisho_default_scroll_fix
// @version      0.1
// @description  Fixes Jisho occasionally loading pages scrolled to the bottom.
// @author       Kuuube
// @run-at       document-start
// @match        https://jisho.org/*
// @icon         https://www.google.com/s2/favicons?sz=64&domain=jisho.org
// ==/UserScript==

function unscroll() {
    window.scrollTo(0, 0);
}

function unscroll_if_bottom() {
    if ((window.innerHeight + window.scrollY) >= document.body.scrollHeight) {
        window.scrollTo(0, 0);
    }
}

(function() {
    unscroll();
    window.addEventListener("DOMContentLoaded", function() {unscroll()});
    window.addEventListener("load", function() {unscroll_if_bottom()});
})();