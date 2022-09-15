export function _makeRequest(url) {
    return function(options) {
        return new Request(url, options);
    }
}

export function _openCache(cacheName) {
    return function() {
        return window.caches.open(cacheName);
    }
}

export function _delete(cacheName) {
    return function() {
        return caches.delete(cacheName);
    }
}

export function _deleteReq(cache) {
    return function(req) {
        return function() {
            return cache.delete(req);
        }
    }
}

export function _add(cache) {
    return function(req) {
        return function() {
            return cache.add(req);
        }
    }
}

export function _match(cache) {
    return function(req) {
        return function() {
            return cache.match(req);
        }
    }
}

export function _fetch(req) {
    return function() {
        return fetch(req);
    }
}
