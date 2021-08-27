'use strict';

const ReactBootstrap = require('react-bootstrap');

if (typeof window !== 'undefined') {
  window.ReactBootstrap = ReactBootstrap;
}

const Alert = require('react-bootstrap/Alert');
const OverlayTrigger = require('react-bootstrap/OverlayTrigger');
const Popover = require('react-bootstrap/Popover');

exports.alertCpt = Alert;
exports.overlayTriggerCpt = OverlayTrigger;
exports.popoverCpt = Popover;
exports.popoverContentCpt = Popover.Content;
exports.popoverTitleCpt = Popover.Title;
