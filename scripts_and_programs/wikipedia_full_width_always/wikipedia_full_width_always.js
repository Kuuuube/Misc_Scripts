// ==UserScript==
// @name         Wikipedia Full Width Always
// @namespace    http://tampermonkey.net/
// @version      0.1
// @description  Automatically set wikipedia pages to full width.
// @author       Kuuube
// @match        *://*.wikipedia.org/*
// @icon         https://www.google.com/s2/favicons?sz=64&domain=wikipedia.org
// ==/UserScript==

// github repo link: https://github.com/Kuuuube/Misc_Scripts/tree/main/scripts_and_programs/wikipedia_full_width_always

function disableLimitedWidth() {
    const featureClassEnabled = 'vector-feature-limited-width-enabled',
          classList = document.body.classList,
          featureClassDisabled = 'vector-feature-limited-width-disabled';
    if (classList.contains(featureClassEnabled)) {
        classList.add(featureClassDisabled);
        classList.remove(featureClassEnabled);
        return false;
    }
}

(function() {
    disableLimitedWidth();
})();