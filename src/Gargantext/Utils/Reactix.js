'use strict';

function addRootElement(rootElem) {
    document.body.insertBefore(
	rootElem,
	document.body.lastElementChild.nextElementSibling
    );
}

function getSelection(_u) {
    return window.getSelection();
}

function stringify(j, indent) {
    return JSON.stringify(j, null, indent);
}

function postMessage(obj, msg, src) {
    obj.contentWindow.postMessage(msg, src);
}

function setCookie(c) {
    document.cookie = c;
}

function domRectFromRect(obj) {
    return DOMRectReadOnly.fromRect(obj)
}

function preventDefault(e) {
    return e.preventDefault();
}

function stopPropagation(e) {
    return e.stopPropagation();
}

function blur(el) {
    return el.blur();
}

function triggerEvent(el, evtType) {
  // https://stackoverflow.com/questions/8789423/trigger-onchange-event
  var event = new UIEvent(evtType, {
    view: window,
    bubbles: true,
    cancelable: true
  });
  el.dispatchEvent(event);
}

exports._addRootElement = addRootElement;
exports._getSelection = getSelection;
exports._stringify = stringify;
exports._postMessage = postMessage;
exports._setCookie = setCookie;
exports._domRectFromRect = domRectFromRect;
exports._preventDefault = preventDefault;
exports._stopPropagation = stopPropagation;
exports._blur = blur;

exports._keyCode = function(e) {
    // https://www.w3schools.com/jsref/event_key_keycode.asp
    return e.which || e.keyCode;
}
exports._triggerEvent = triggerEvent;
