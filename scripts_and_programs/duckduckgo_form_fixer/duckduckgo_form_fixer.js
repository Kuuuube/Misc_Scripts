// ==UserScript==
// @name         Duckduckgo Form Fixer
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/duckduckgo_form_fixer
// @version      0.1
// @description  Forces searching from the URL instead of POST when using html.duckduckgo.
// @author       Kuuube
// @match        https://html.duckduckgo.com/html/*
// @icon         https://www.google.com/s2/favicons?sz=64&domain=duckduckgo.com
// ==/UserScript==

function header_form() {
    var header_form = document.getElementsByClassName("header__form");
    for (var i = 0; i < header_form.length; i++) {
        header_form[i].removeAttribute("method")
    }
}

function homepage_form() {
    var search_form_homepage = document.getElementsByClassName("search");
    for (var i = 0; i < search_form_homepage.length; i++) {
        search_form_homepage[i].removeAttribute("method")
    }
}

(function() {
    header_form();
    homepage_form();
})();