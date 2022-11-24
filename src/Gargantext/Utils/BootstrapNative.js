"use strict";

export function createDropdown(iid) {
    var el = document.getElementById(iid);

    if (!window.Dropdown) return;

    new window.Dropdown(el, {});
};
