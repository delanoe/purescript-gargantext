'use strict';

import API from '../../src/external-deps/JitsiMeetAPI.js';

// console.log('API', API);

export { API as _api };
export function _jitsiMeetAPI(host, options) {
    return new API(host, options);
};
