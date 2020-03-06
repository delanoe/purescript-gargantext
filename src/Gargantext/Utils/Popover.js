'use strict';

const popover = require('react-awesome-popover');

if (typeof window !== 'undefined') {
    window.Popover = popover;
}

exports.popoverCpt = popover;
