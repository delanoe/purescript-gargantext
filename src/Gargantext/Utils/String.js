exports._btoa = function(s) {
    return btoa(unescape(encodeURIComponent(s)));
}
