'use strict';

export function _addRootElement(rootElem) {
    document.body.insertBefore(
        rootElem,
        document.body.lastElementChild.nextElementSibling
    );
}

export function _getSelection(_u) {
    return window.getSelection();
}

export function _stringify(j, indent) {
    return JSON.stringify(j, null, indent);
}

export function _postMessage(obj, msg, src) {
    obj.contentWindow.postMessage(msg, src);
}

export function _setCookie(c) {
    document.cookie = c;
}

export function _domRectFromRect(obj) {
    return DOMRectReadOnly.fromRect(obj)
}

export function _preventDefault(e) {
    return e.preventDefault();
}

export function _stopPropagation(e) {
    return e.stopPropagation();
}

export function _blur(el) {
    return el.blur();
}

export function _triggerEvent(el, evtType) {
  // https://stackoverflow.com/questions/8789423/trigger-onchange-event
  var event = new UIEvent(evtType, {
    view: window,
    bubbles: true,
    cancelable: true
  });
  el.dispatchEvent(event);
}

export function _keyCode(e) {
    // https://www.w3schools.com/jsref/event_key_keycode.asp
    return e.which || e.keyCode;
}
