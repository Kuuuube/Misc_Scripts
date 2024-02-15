// ==UserScript==
// @name         Wikipedia No Fundraising
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/wikipedia_no_fundraising
// @version      0.3
// @description  Automatically remove wikipedia fundraising popups.
// @author       Kuuube
// @match        *://*.wikipedia.org/*
// @icon         https://www.google.com/s2/favicons?sz=64&domain=wikipedia.org
// ==/UserScript==

function setHideCookie() {
    document.cookie = "centralnotice_hide_fundraising={\"v\":1,\"created\":9999999999,\"reason\":\"close\"}"
    document.cookie = "centralnotice_hide_WikiLovesFolklore2024International={\"v\":1,\"created\":9999999999,\"reason\":\"close\"}"
}

(function() {
    setHideCookie();
})();