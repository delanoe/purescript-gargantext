'use strict';

const sigma = require('sigma/src/garg.js').sigma;

if (typeof window !== 'undefined') {
  window.sigma = sigma;
}

require('sigma/plugins/garg.js').init(sigma, window);

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
function refresh(sigma) { sigma.refresh(); }
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
function getRendererContainer(sigma) {
  return sigma.renderers[0].container;
}
function setRendererContainer(sigma, el) {
  sigma.renderers[0].container = el;
}
function killSigma(left, right, sigma) {
  try {
    sigma.kill()
    return right(null)
  } catch(e) {
    return left(e);
  }
}
function clear(sigma) { sigma.graph.clear(); }
function bind(sigma, event, handler) { sigma.bind(event, handler); }
function setSettings(sigma, settings) { sigma.settings(settings); }
function startForceAtlas2(sigma, settings) { sigma.startForceAtlas2(settings); }
function stopForceAtlas2(sigma) { sigma.stopForceAtlas2(); }
function killForceAtlas2(sigma) { sigma.killForceAtlas2(); }
function isForceAtlas2Running(sigma) { return sigma.isForceAtlas2Running(); }

function getCameras(sigma) {
  // For some reason, sigma.cameras is an object with integer keys
  return Object.values(sigma.cameras);
};

function goTo(cam, props) {
  return cam.goTo(props);
};

exports._sigma = _sigma;
exports._graphRead = graphRead;
exports._refresh = refresh;
exports._addRenderer = addRenderer;
exports._killRenderer = killRenderer;
exports._getRendererContainer = getRendererContainer;
exports._setRendererContainer = setRendererContainer;
exports._killSigma = killSigma
exports._clear = clear;
exports._bind = bind;
exports._setSettings = setSettings;
exports._startForceAtlas2 = startForceAtlas2;
exports._stopForceAtlas2 = stopForceAtlas2;
exports._killForceAtlas2 = killForceAtlas2;
exports._isForceAtlas2Running = isForceAtlas2Running;
exports._getCameras = getCameras;
exports._goTo = goTo;
