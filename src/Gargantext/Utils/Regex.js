function _cloneRegex(r) {
  return new RegExp(r.source, r.flags);
}
function _getRegexLastIndex(r) {
  return r.lastIndex;
}
function _execRegex(r, s) {
  return r.exec(s);
}
exports._cloneRegex = _cloneRegex;
exports._getRegexLastIndex = _getRegexLastIndex;
exports._execRegex = _execRegex;
