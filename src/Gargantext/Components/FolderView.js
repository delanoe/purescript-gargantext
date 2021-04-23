'use strict';

exports.back = function() {
  return function() {
    history.back();
  }
}