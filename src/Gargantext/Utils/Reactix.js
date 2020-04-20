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

exports._addRootElement = addRootElement;
exports._getSelection = getSelection;
exports._stringify = stringify;
exports._postMessage = postMessage;
exports._setCookie = setCookie;
