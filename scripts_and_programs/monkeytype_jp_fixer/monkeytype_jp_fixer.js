// ==UserScript==
// @name         Monkeytype JP Fixer
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/monkeytype_jp_fixer
// @version      0.1
// @description  Automatically inserts a space when after enter is pressed to make typing Japanese less awkward.
// @author       Kuuube
// @match        *://monkeytype.com/
// @icon         https://www.google.com/s2/favicons?sz=64&domain=monkeytype.com
// @grant        none
// ==/UserScript==

(function() {
    window.addEventListener("keydown", function(event) {
        if (event.code === "Enter") {
            document.getElementById("wordsInput").value += " ";
        }
    });
})();