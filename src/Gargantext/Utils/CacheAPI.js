exports._makeRequest = function() {
    return function(url) {
        return function(options) {
            return new Request(url, options);

        }
    }
}

exports._openCache = function(cacheName) {
    return function() {
        return window.caches.open(cacheName);
    }
}

exports._delete = function(cacheName) {
    return function() {
        return caches.delete(cacheName);
    }
}

exports._deleteReq = function(cache) {
    return function(req) {
        return function() {
            return cache.delete(req);
        }
    }
}

exports._add = function(cache) {
    return function(req) {
        return function() {
            return cache.add(req);
        }
    }
}

exports._match = function(cache) {
    return function(req) {
        return function() {
            return cache.match(req);
        }
    }
}

exports._fetch = function(req) {
    return function() {
        return fetch(req);
    }
}
