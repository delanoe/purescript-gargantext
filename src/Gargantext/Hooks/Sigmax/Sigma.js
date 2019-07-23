'use strict';

const s = require('sigma').sigma;

if (typeof window === 'undefined') {
  window.sigma = s;
}

function _sigma(left, right, opts) {
  try {
    return right(new sigma(opts));
  } catch(e) {
    return left(e);
  }
}

function graphRead(left, right, sigma, data) {
  try {
    return right(sigma.graph.read(data));
  } catch(e) {
    return left(e);
  }
}
function addRenderer(left, right, sigma, renderer) {
  try {
    return right(sigma.addRenderer(renderer));
  } catch(e) {
    return left(e);
  }
}
function killRenderer(left, right, sigma, renderer) {
  try {
    sigma.killRenderer(renderer);
    return right(sigma)
  } catch(e) {
    return left(e);
  }
}
function killSigma(left, right, sigma) {
  try {
    sigma.kill()
    return right(null)
  } catch(e) {
    return left(e);
  }
}

exports._sigma = _sigma;
exports._graphRead = graphRead;
exports._refresh = function refresh(sigma) { sigma.refresh(); };
exports._addRenderer = addRenderer;
exports._killRenderer = killRenderer;
exports._killSigma = killSigma
exports._clear = function clear(sigma) { sigma.graph.clear(); };
exports._bind = function bind(sigma, event, handler) { sigma.bind(event, handler); };
exports._startForceAtlas2 = function startForceAtlas2(sigma, settings) {
  sigma.startForceAtlas2(settings);
};
exports._stopForceAtlas2 = function stopForceAtlas2(sigma) { sigma.stopForceAtlas2(); };
exports._killForceAtlas2 = function killForceAtlas2(sigma) { sigma.killForceAtlas2(); };
