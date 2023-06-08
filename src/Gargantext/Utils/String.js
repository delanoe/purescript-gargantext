export function _btoa(s) {
  return btoa(unescape(encodeURIComponent(s)));
}

export function _specialCharNormalize(s) {
  return s.normalize('NFD').replace(/[\u0300-\u036f]/g, '');
}
