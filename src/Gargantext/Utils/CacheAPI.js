exports._makeRequest = function() {
    return function(url) {
        return function(options) {
            console.log('[_makeRequest] url', url);
            console.log('[_makeRequest] options', options);
            return new Request(url, options);

        }
    }
}

exports._openCache = function(cacheName) {
    return function() {
        return caches.open(cacheName);
    }
}

exports._cached = function(cache) {
    return function(req) {
        return function(onError, onSuccess) {
            cache.match(req).then(function(res) {
                if (res) {
                    console.log('[_getC] cache hit with', req);
                    onSuccess(res)
                } else {
                    cache.add(req).then(function(res) {
                        console.log('[_getC] cache miss with', req);
                        onSuccess(res);
                    }, function(err) {
                        onError(err);
                    })
                }
            }, function(err) {
                onError(err);
            })

            return function(cancelError, onCancelerError, onCancelerSuccess) {
                onCancelerSuccess();
            }
        }
    }
}

exports._delete = function(cache) {
    return function(req) {
        return function() {
            cache.delete(req);
        }
    }
}
