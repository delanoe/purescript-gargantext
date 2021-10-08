'use strict';

exports._addClassName = function(window, className) {
  window.document.body.classList.add(className);
}

exports._removeClassName = function(window, className) {
  window.document.body.classList.remove(className);
}
