// ==UserScript==
// @name         Monkeytype JP Fixer
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/monkeytype_jp_fixer
// @version      0.2
// @description  Automatically inserts a space when after enter is pressed to make typing Japanese less awkward.
// @author       Kuuube
// @match        *://monkeytype.com/
// @icon         https://www.google.com/s2/favicons?sz=64&domain=monkeytype.com
// ==/UserScript==

(function() {
    window.addEventListener("keydown", function(event) {
        if (event.code === "Enter" && document.getElementById("wordsInput").value.slice(-1) != " ") {
            document.getElementById("wordsInput").value += " ";
        }
    });
    window.addEventListener("compositionend", function(event) {
        if (document.getElementById("wordsInput").value.slice(-1) != " ") {
            document.getElementById("wordsInput").value += " ";
        }
    });
})();
