'use strict';

import Graph from 'graphology';
import Sigma from 'sigma';

let sigma = Sigma.Sigma;
console.log('imported sigma', Sigma);

if (typeof window !== 'undefined') {
  window.sigma = Sigma;
}

/*import('sigma/plugins/garg.js').then((module) => {
  let CustomShapes = module.init(sigma, window).customShapes;
  CustomShapes.init();
  });
  */
//import('sigma/src/utils/sigma.utils.js').then((module) => { module.init(sigma) });

// Black circle around a node
/*
(function() {
  let originalDef = sigma.canvas.nodes.def;

  sigma.canvas.nodes.def = (node, context, settings) => {
    let prefix = settings('prefix') || '';

    originalDef(node, context, settings);

    context.strokeStyle = '#000';
    context.lineWidth = 1;
    context.beginPath();
    context.arc(
      node[prefix + 'x'],
      node[prefix + 'y'],
      node[prefix + 'size'],
      0,
      Math.PI * 2,
      true
    );
    context.stroke();
  }
})()
*/

/*
sigma.canvas.nodes.selected = (node, context, settings) => {
  // hack
  // We need to temporarily set node.type to 'def'. This is for 2 reasons
  // 1. Make it render as a normal node
  // 2. Avoid infinite recursion (hovers.def calls node renderer and we would end up here back
  //    again with node.type = 'hovered')
  node.type = 'def';
  sigma.canvas.hovers.def(node, context, settings);
  node.type = 'selected';
  //console.log('hovers, settings:', settings);
  };
*/

//CustomShapes.init();

/*
let sigmaMouseSelector = function(sigma, options) {
  sigma.plugins = sigma.plugins || {};

  sigma.plugins.mouseSelector = (s, renderer) => {
    let _self = this;
    let _offset = null;
    const _s = s;
    const _renderer = renderer;
    const _container = _renderer.container;
    //renderer.initDOM('canvas', 'mouseSelector');
    // A hack to force resize to be called (there is a width/height equality
    // check which can't be escaped in any other way).
    //renderer.resize(renderer.width - 1, renderer.height - 1);
    //renderer.resize(renderer.width + 1, renderer.height + 1);
    const _context = _renderer.contexts.mouseSelector;
    // These are used to prevent using the 'click' event when in fact this was a drag
    let _clickPositionX = null;
    let _clickPositionY = null;
    let _isValidClick = false;

    _container.onmousemove = function(e) { return mouseMove(e); };
    _context.canvas.onclick = function(e) { return onClick(e); };
    _container.onmousedown = function(e) { return onMouseDown(e); }
    _container.onmouseup = function(e) { return onMouseUp(e); }
    s.bind('click', function(e) { return onClick(e); })
    // The mouseSelector canvas will pass its events down to the "mouse" canvas.
    _context.canvas.style.pointerEvents = 'none';

    s.bind('kill', () => _self.unbindAll());

    this.unbindAll = () => {
      // console.log('[sigmaMouseSelector] unbinding');
      _container.onclick = null;
      _context.canvas.onmousemove = null;
      _container.onmousedown = null;
      _container.onmouseup = null;
    }

    const onMouseDown = (e) => {
      _clickPositionX = e.clientX;
      _clickPositionY = e.clientY;
    }

    const onMouseUp = (e) => {
      // Prevent triggering click when in fact this was a drag
      if ((_clickPositionX != e.clientX) || (_clickPositionY != e.clientY)) {
        _clickPositionX = null;
        _clickPositionY = null;
        _isValidClick = false;
      } else {
        _isValidClick = true;
      }
    }

    const mouseMove = (e) => {
      const size = _s.settings('mouseSelectorSize') || 3;
      const x = e.clientX + document.body.scrollLeft - _offset.left - size/2;
      const y = e.clientY + document.body.scrollTop - _offset.top - size/2;
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
      if(!_isValidClick) {
        return;
      }
      const size = _s.settings('mouseSelectorSize') || 3;
      const x = e.data.clientX + document.body.scrollLeft - _offset.left - size/2;
      const y = e.data.clientY + document.body.scrollTop - _offset.top - size/2;
      const prefix = _renderer.options.prefix;
      //console.log('[sigmaMouseSelector] clicked', e, x, y, size);
      let nodes = [];
      _s.graph.nodes().forEach((node) => {
        const nodeX = node[prefix + 'x'];
        const nodeY = node[prefix + 'y'];
        if(sigma.utils.getDistance(x, y, nodeX, nodeY) <= size) {
          nodes.push(node);
        }
      });
      //console.log('[sigmaMouseSelector] nodes', nodes);
      _renderer.dispatchEvent('clickNodes', {
        node: nodes,
        captor: e.data
      })
      _clickPositionX = null;
      _clickPositionY = null;
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

    // Container resize event listener
    // @TODO: debounce?
    const onContainerResize = (entries) => {
      _offset = calculateOffset(_container);
    };
    const _resizeObserver = new ResizeObserver( onContainerResize );
    _resizeObserver.observe(_container);
  }
}

sigmaMouseSelector(sigma);
*/

function _sigma(left, right, el, opts) {
  try {
    let graph = new Graph();
    console.log('initializing sigma with el', el);
    console.log('initializing sigma with opts', opts);
    return right(new sigma(graph, el, opts));
  } catch(e) {
    return left(e);
  }
}

function _addRenderer(left, right, sigma, renderer) {
  try {
    return right(sigma.addRenderer(renderer));
  } catch(e) {
    return left(e);
  }
}
function _bindMouseSelectorPlugin(left, right, sig) {
  try {
    return right(sigma.plugins.mouseSelector(sig, sig.renderers[0]));
  } catch(e) {
    console.log('[bindMouseSelectorPlugin] error', e);
    return left(e);
  }
}
function _on(sigma, event, handler) { sigma.on(event, handler); }

function _takeScreenshot(sigma) {
  let c = sigma.renderers[0].container;
  let edges = c.getElementsByClassName('sigma-edges')[0];
  let scene = c.getElementsByClassName('sigma-scene')[0];
  // let sceneCtx = scene.getContext('2d');
  // sceneCtx.globalAlpha = 1;
  // sceneCtx.drawImage(edges, 0, 0);
  // return scene.toDataURL('image/png');
  let edgesCtx = edges.getContext('2d');
  edgesCtx.globalAlpha = 1;
  edgesCtx.drawImage(scene, 0, 0);
  return edges.toDataURL('image/png');
}

function _getEdges(sigma) {
  return sigma.graph.edges();
}

function _getNodes(sigma) {
  return sigma.graph.nodes();
}

function _proxySetSettings(window, sigma, settings) {
  var id = sigma.id;

  window.sigma.instances(id).settings(settings);
  window.sigma.instances(id).refresh();
}

let dummy = function() {};

let _setSettings = function(g, settings) {
  for(const key in settings) {
    g.setSetting(key, settings[key]);
  }
}

let _refresh = function(g) {
  console.log('[refresh], g', g);
  return g.refresh();
}

export { _sigma,
         _addRenderer,
         dummy as _bindMouseSelectorPlugin,
         _on,
         _takeScreenshot,
         _getEdges,
         _getNodes,
         _proxySetSettings,
         _setSettings,
         _refresh };
