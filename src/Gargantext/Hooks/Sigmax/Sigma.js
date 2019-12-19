'use strict';

const sigma = require('sigma/src/garg.js').sigma;

if (typeof window !== 'undefined') {
  window.sigma = sigma;
}

const CustomShapes = require('sigma/plugins/garg.js').init(sigma, window).customShapes;
require('sigma/src/utils/sigma.utils.js').init(sigma);

sigma.canvas.nodes.selected = (node, context, settings) => {
  // hack
  // We need to temporarily set node.type to 'def'. This is for 2 reasons
  // 1. Make it render as a normal node
  // 2. Avoid infinite recursion (hovers.def calls node renderer and we would end up here back
  //    again with node.type = 'hovered')
  node.type = 'def';
  sigma.canvas.hovers.def(node, context, settings);
  node.type = 'selected';
};

CustomShapes.init();

let sigmaMouseSelector = (sigma, options) => {
  sigma.plugins = sigma.plugins || {};

  sigma.plugins.mouseSelector = (s, renderer) => {
    var _self = this;
    var _offset = null;
    const _s = s;
    const _renderer = renderer;
    const _container = _renderer.container;
    //renderer.initDOM('canvas', 'mouseSelector');
    // A hack to force resize to be called (there is a width/height equality
    // check which can't be escaped in any other way).
    //renderer.resize(renderer.width - 1, renderer.height - 1);
    //renderer.resize(renderer.width + 1, renderer.height + 1);
    const _context = _renderer.contexts.mouseSelector;

    _container.onmousemove = function(e) { return mouseMove(e); };
    _context.canvas.onclick = function(e) { return onClick(e); };
    s.bind('click', function(e) { return onClick(e); })
    // The mouseSelector canvas will pass its events down to the "mouse" canvas.
    _context.canvas.style.pointerEvents = 'none';

    s.bind('kill', () => _self.unbindAll());

    this.unbindAll = () => {
      console.log('[sigmaMouseSelector] unbinding');
      _container.onclick = null;
      _container.onmousemove = null;
    }

    const mouseMove = (e) => {
      const size = _s.settings('mouseSelectorSize') || 3;
      const x = e.clientX - _offset.left - size/2;
      const y = e.clientY - _offset.top - size/2;
      _context.clearRect(0, 0, _context.canvas.width, _context.canvas.height);

      _context.fillStyle = 'rgba(91, 192, 222, 0.7)';
      _context.beginPath();
      _context.arc(
        x,
        y,
        size,
        0,
        Math.PI * 2,
        true
      );
      _context.closePath();
      _context.fill();
    }

    const onClick = (e) => {
      const size = _s.settings('mouseSelectorSize') || 3;
      const x = e.data.clientX - _offset.left - size/2;
      const y = e.data.clientY - _offset.top - size/2;
      const prefix = _renderer.options.prefix;
      console.log('[sigmaMouseSelector] clicked', e, x, y, size);
      let nodes = [];
      _s.graph.nodes().forEach((node) => {
        const nodeX = node[prefix + 'x'];
        const nodeY = node[prefix + 'y'];
        if(sigma.utils.getDistance(x, y, nodeX, nodeY) <= size) {
          nodes.push(node);
        }
      });
      console.log('[sigmaMouseSelector] nodes', nodes);
    }

    const calculateOffset = (element) => {
      var style = window.getComputedStyle(element);
      var getCssProperty = function(prop) {
        return parseInt(style.getPropertyValue(prop).replace('px', '')) || 0;
      };
      return {
        left: element.getBoundingClientRect().left + getCssProperty('padding-left'),
        top: element.getBoundingClientRect().top + getCssProperty('padding-top')
      };
    };

    _offset = calculateOffset(renderer.container);

  }
}

sigmaMouseSelector(sigma);

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
function bindMouseSelectorPlugin(left, right, sig) {
  try {
    return right(sigma.plugins.mouseSelector(sig, sig.renderers[0]));
  } catch(e) {
    console.log('[bindMouseSelectorPlugin] error', e);
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
function unbind(sigma, event) { sigma.unbind(event); }
function forEachNode(sigma, handler) { sigma.graph.nodes().forEach(handler); }
function forEachEdge(sigma, handler) { sigma.graph.edges().forEach(handler); }
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
exports._bindMouseSelectorPlugin = bindMouseSelectorPlugin;
exports._killRenderer = killRenderer;
exports._getRendererContainer = getRendererContainer;
exports._setRendererContainer = setRendererContainer;
exports._killSigma = killSigma
exports._clear = clear;
exports._bind = bind;
exports._unbind = unbind;
exports._forEachNode = forEachNode;
exports._forEachEdge = forEachEdge;
exports._setSettings = setSettings;
exports._startForceAtlas2 = startForceAtlas2;
exports._stopForceAtlas2 = stopForceAtlas2;
exports._killForceAtlas2 = killForceAtlas2;
exports._isForceAtlas2Running = isForceAtlas2Running;
exports._getCameras = getCameras;
exports._goTo = goTo;
