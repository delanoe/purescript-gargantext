'use strict';
exports.goToImpl = function(cam) {
  return function(props) {
    return cam.goTo(props);
  };
};
