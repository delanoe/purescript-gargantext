'use strict';

var API = require('../../src/external-deps/JitsiMeetAPI.js');

console.log('API', API);

exports._api = API;
exports._jitsiMeetAPI = function(host, options) {
    return new API(host, options);
};
