'use strict';

const ReactBootstrap = require('react-bootstrap');

if (typeof window !== 'undefined') {
  window.ReactBootstrap = ReactBootstrap;
}

const OverlayTrigger = require('react-bootstrap/OverlayTrigger');
const Popover = require('react-bootstrap/Popover');

exports.overlayTriggerCpt = OverlayTrigger;
exports.popoverCpt = Popover;
exports.popoverContentCpt = Popover.Content;
exports.popoverTitleCpt = Popover.Title;
