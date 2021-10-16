// shameless copy from
// https://stackoverflow.com/a/9458996
// This is because base64-codec/Data.Base64.encodeBase64 was too slow

exports.arrayBufferToBase64Impl = function(buffer) {
    var binary = '';
    var bytes = new Uint8Array( buffer );
    var len = bytes.byteLength;
    for (var i = 0; i < len; i++) {
        binary += String.fromCharCode( bytes[ i ] );
    }
    return window.btoa( binary );
}
