// ==UserScript==
// @name         RTINGS Bypass
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/rtings_bypass
// @version      0.1
// @description  Disables RTINGS' three review limit.
// @author       Kuuube
// @run-at       document-start
// @match        *://*.rtings.com/*
// @icon         https://www.google.com/s2/favicons?sz=64&domain=rtings.com
// ==/UserScript==

function resetCookie() {
    var cookieList = document.cookie.split (/;\s*/);
    for (var J = cookieList.length - 1; J >= 0; --J) {
        var cookieName = cookieList[J].replace(/\s*(\w+)=.+$/, "$1");
        eraseCookie(cookieName);
    }
}

function eraseCookie (cookieName) {
    var pathNodes = location.pathname.split("/").map(function(pathWord) {
        return '/' + pathWord;
    });
    var cookPaths = [""].concat(pathNodes.map(function(pathNode) {
        if (this.pathStr) {
            this.pathStr += pathNode;
        } else {
            this.pathStr = "; path=";
            return this.pathStr + pathNode;
        }
        return this.pathStr;
    }));

    (eraseCookie = function(cookieName) {
        cookPaths.forEach(function(pathStr) {
            var diagStr = cookieName + "=" + pathStr + "; expires=Thu, 01-Jan-1970 00:00:01 GMT;";
            document.cookie = diagStr;
            document.cookie = cookieName + "=" + pathStr + "; domain=" + location.hostname + "; expires=Thu, 01-Jan-1970 00:00:01 GMT;";
            document.cookie = cookieName + "=" + pathStr + "; domain=" + location.hostname.replace(/^www\./, "") + "; expires=Thu, 01-Jan-1970 00:00:01 GMT;";
            document.cookie = cookieName + "=" + pathStr + "; domain=" + location.hostname.replace(/^(\w+\.)+?(\w+\.\w+)$/, "$2") + "; expires=Thu, 01-Jan-1970 00:00:01 GMT;";
        });
    })(cookieName);
}

(function() {
    resetCookie();
})();