// ==UserScript==
// @name         Wikipedia Full Width Always
// @namespace    https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/wikipedia_full_width_always
// @version      0.3
// @description  Automatically set wikipedia pages to full width.
// @author       Kuuube
// @match        *://*.wikipedia.org/*
// @icon         https://www.google.com/s2/favicons?sz=64&domain=wikipedia.org
// ==/UserScript==

function disableLimitedWidth() {
    const featureClassEnabled = "vector-feature-limited-width-enabled";
    const featureClassDisabled = "vector-feature-limited-width-disabled";
    const classList = document.documentElement.classList;

    if (classList.contains(featureClassEnabled)) {
        classList.remove(featureClassEnabled);
    }

    if (!classList.contains(featureClassDisabled)) {
        classList.add(featureClassDisabled);
    }
}

(function() {
    disableLimitedWidth();
})();