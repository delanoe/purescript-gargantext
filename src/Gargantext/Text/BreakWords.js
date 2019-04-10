exports._wordRegex = /[a-z]+/gi;
exports._cloneRegex = function(r) { return new RegExp(r.source, r.flags); };
exports._getRegexLastIndex = function(r) { return r.lastIndex; };
exports._execRegex = function(r, s) { return r.exec(s); };
