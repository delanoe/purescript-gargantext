'use strict';

/**
 * @param {DOMElement} element
 * @param {string} attribute
 * @param {string} value
 * @returns
 */
exports.setAttribute = function(element) {
  return function(attribute) {
    return function(value) {
      return function() {
        element.setAttribute(attribute, value);
      }
    }
  }
}
