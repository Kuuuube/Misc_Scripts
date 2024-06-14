// ==UserScript==
// @name         Wikipedia Full Width Always
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/wikipedia_full_width_always
// @version      0.6
// @description  Automatically set wikipedia pages to full width and remove sidebar.
// @author       Kuuube
// @match        *://*.wikipedia.org/*
// @icon         https://www.google.com/s2/favicons?sz=64&domain=wikipedia.org
// ==/UserScript==

function disableLimitedWidth() {
    const featureClassEnabled = "vector-feature-limited-width-clientpref-1";
    const featureClassDisabled = "vector-feature-limited-width-clientpref-0";
    const classList = document.documentElement.classList;

    if (classList.contains(featureClassEnabled)) {
        classList.remove(featureClassEnabled);
    }

    if (!classList.contains(featureClassDisabled)) {
        classList.add(featureClassDisabled);
    }
}

function disableSidebar() {
    const featureClassEnabled = "vector-feature-appearance-pinned-clientpref-1";
    const featureClassDisabled = "vector-feature-appearance-pinned-clientpref-0";
    const classList = document.documentElement.classList;

    if (classList.contains(featureClassEnabled)) {
        classList.remove(featureClassEnabled);
    }

    if (!classList.contains(featureClassDisabled)) {
        classList.add(featureClassDisabled);
    }
}

function setCookie() {
    document.cookie = "enwikimwclientpreferences=vector-feature-limited-width-clientpref-0,vector-feature-appearance-pinned-clientpref-0"
    document.cookie = "enwikilimited-width-aware=1";
}

(function() {
    disableLimitedWidth();
    disableSidebar();
    setCookie();
})();
