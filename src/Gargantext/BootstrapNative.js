"use strict";

exports.createDropdown = function(iid) {
    var el = document.getElementById(iid);

    if (!window.Dropdown) return;

    new window.Dropdown(el, {});
};
