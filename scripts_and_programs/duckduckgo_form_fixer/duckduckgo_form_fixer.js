// ==UserScript==
// @name         Duckduckgo Form Fixer
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/duckduckgo_form_fixer
// @version      0.1
// @description  Forces searching from the URL instead of POST when using html.duckduckgo. This allows proper use of the browser back button.
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

(function() {
    header_form();
    homepage_form();
})();