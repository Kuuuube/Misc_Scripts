// ==UserScript==
// @name         Jisho Default Scroll Fix
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/jisho_default_scroll_fix
// @version      0.2
// @description  Fixes Jisho occasionally loading pages scrolled to the bottom.
// @author       Kuuube
// @match        https://jisho.org/*
// @icon         https://www.google.com/s2/favicons?sz=64&domain=jisho.org
// ==/UserScript==

(function() {
    window.addEventListener("load", function() {window.scrollTo(0, 0);});
})();