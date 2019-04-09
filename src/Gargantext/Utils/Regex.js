function _cloneRegex(r) { return new RegExp(r.source, r.flags); }
function _getRegexLastIndex(r) { return r.lastIndex; }
function _execRegex(r, s) { return r.exec(s); }
module.exports={
    _cloneRegex: _cloneRegex,
    _getRegexLastIndex: _getRegexLastIndex,
    _execRegex: _execRegex
};
