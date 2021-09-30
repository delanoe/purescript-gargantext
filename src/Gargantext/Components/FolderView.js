'use strict';

exports.back = function() {
  return function() {
    history.back();
  }
}

exports.link = function (url) {
  return function() {
    window.location.href = url
  }
}