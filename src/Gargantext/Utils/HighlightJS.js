'use strict';

const hljs = require('highlightjs/highlight.pack.min.js');

function highlightBlock(el) {
    hljs.highlightBlock(el);
}

exports._highlightBlock = highlightBlock;
