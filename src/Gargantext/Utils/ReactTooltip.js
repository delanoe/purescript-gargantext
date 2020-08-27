'use strict';

const reactTooltip = require('react-tooltip');

if (typeof window !== 'undefined') {
    window.ReactTooltip = reactTooltip;
}

exports.reactTooltipCpt = reactTooltip;
