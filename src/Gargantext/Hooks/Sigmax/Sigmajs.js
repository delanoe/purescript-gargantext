'use strict';

export function goToImpl(cam) {
  return function(props) {
    return cam.goTo(props);
  };
};
