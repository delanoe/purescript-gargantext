'use strict';

import popover from 'react-awesome-popover';

if (typeof window !== 'undefined') {
    window.Popover = popover;
}

export { popover as popoverCpt };
export function _setState(el, val) {
    el.setState(val);
}
