'use strict';

const popover = require('react-awesome-popover');

if (typeof window !== 'undefined') {
    window.Popover = popover;
}

exports.popoverCpt = popover;
exports._setState = function(el, val) {
    el.setState(val);
}
