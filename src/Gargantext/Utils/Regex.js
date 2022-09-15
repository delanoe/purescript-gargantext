export function _cloneRegex(r) {
  return new RegExp(r.source, r.flags);
}
export function _getRegexLastIndex(r) {
  return r.lastIndex;
}
export function _execRegex(r, s) {
  return r.exec(s);
}
