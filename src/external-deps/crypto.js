// https://siteultra.com/blog/using-subtle-in-a-javascript-module-from-both-the-browser-and-nodejs

let subtle = undefined;

if (typeof window !== 'undefined' && window.crypto && window.crypto.subtle) {
   subtle = window.crypto.subtle;
} else {
   // Import 'crypto' module in Node.js environment
   subtle = require('crypto').subtle;
}

exports.subtle = subtle;
