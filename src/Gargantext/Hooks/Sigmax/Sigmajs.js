'use strict';
exports.goToImpl = function(cam) {
  return function(props) {
    return cam.goTo(props);
  };
};
exports.pauseForceAtlas2 = function() {
  var s = window.sigmaGargInstance;
  if (s) {
    if (s.isForceAtlas2Running()) {
      s.stopForceAtlas2()
    }
    else {
      s.startForceAtlas2()
    }
  }
};
