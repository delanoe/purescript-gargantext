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

exports._addRootElement = addRootElement;
exports._getSelection = getSelection;
