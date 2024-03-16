// ==UserScript==
// @name         Duckduckgo Form Fixer
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/duckduckgo_form_fixer
// @version      0.2
// @description  Forces searching from the URL instead of POST when using html.duckduckgo.
// @author       Kuuube
// @match        https://html.duckduckgo.com/html*
// @icon         https://www.google.com/s2/favicons?sz=64&domain=duckduckgo.com
// ==/UserScript==

function header_form() {
    var header_form_element = document.getElementsByClassName("header__form");
    for (var i = 0; i < header_form_element.length; i++) {
        header_form_element[i].removeAttribute("method")
    }
}

function homepage_form() {
    var homepage_form_element = document.getElementsByClassName("search");
    for (var i = 0; i < homepage_form_element.length; i++) {
        homepage_form_element[i].removeAttribute("method")
    }
}

function next_page_form() {
    var next_page_form_element = document.getElementsByClassName("nav-link");
    for (var i = 0; i < next_page_form_element.length; i++) {
        for (var j = 0; j < next_page_form_element[i].children.length; j++) {
            next_page_form_element[i].children[j].removeAttribute("method")
        }
    }
}

(function() {
    header_form();
    homepage_form();
    next_page_form();
})();