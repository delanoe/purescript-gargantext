'use strict';

import Graph from 'graphology';
import Sigma from 'sigma';
import { takeScreenshot } from '../../src/external-deps/sigmajs-screenshot.js';
import CircleNodeProgram from 'sigma/rendering/webgl/programs/node.fast';
import ContourCircleNodeProgram from '../../src/external-deps/sigmajs-circle-with-contour.js';
import TriangleNodeProgram from '../../src/external-deps/sigmajs-triangle.js';
import ContourTriangleNodeProgram from '../../src/external-deps/sigmajs-triangle-with-contour.js';
import SquareNodeProgram from '../../src/external-deps/sigmajs-square.js';
import ContourSquareNodeProgram from '../../src/external-deps/sigmajs-square-with-contour.js';

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

let sigmaMouseSelector = function(sigma, options) {
  const distance = (x1, y1, x2, y2) => Math.sqrt(Math.pow(x1 - x2, 2) + Math.pow(y1 - y2, 2));

  let mouseSelector = () => {
    const _self = this;
    const _s = sigma;
    const captor = sigma.mouseCaptor;
    const _container = captor.container;
    const _context = _container.getContext('2d');

    const unbindAll = () => {
      // TODO Maybe not needed if sigma is killed and we did bind to
      // mouse captor instead of the canvas?

      // _container.onclick = null;
      // _container.onmousemove = null;
      // _container.onmousedown = null;
      // _container.onmouseup = null;
    }

    const bindAll = () => {
      captor.on('mousemove', mouseMove);
      captor.on('click', onClick);
      captor.on('wheel', onWheel);

      sigma.on('kill', () => unbindAll());
    }

    const onWheel = (e) => {
      const shiftPressed = e.original.shiftKey;
      // zoom in has e.delta > 0 (around 0.44)
      // zoom out has e.delta < 0 (around -0.44)
      if(shiftPressed) {
        // TODO Fix this so that the canvas is not zoomed.
        console.log('[onWheel] e', e);
        e.original.preventDefault();
        e.original.stopPropagation();
        sigma.emit('shiftWheel', {
          delta: e.delta
        });
      }
    }

    // Responsible for rendering the selector properly
    const mouseMove = (e) => {
      const size = sigma.settings['mouseSelectorSize'] || 3;
      _context.clearRect(0, 0, _context.canvas.width, _context.canvas.height);

      _context.fillStyle = 'rgba(91, 192, 222, 0.7)';
      _context.beginPath();
      _context.arc(
        e.x,
        e.y,
        size,
        0,
        Math.PI * 2,
        true
      );
      _context.closePath();
      _context.fill();
    }

    const onClick = (e) => {
      const size = sigma.settings['mouseSelectorSize'] || 3;
      let nodeIds = [];
      for(let nodeId in sigma.nodeDataCache) {
        let data = sigma.nodeDataCache[nodeId];
        let position = sigma.framedGraphToViewport(data);
        // TODO Either distance or node is clicked directly
        if(distance(e.x, e.y, position.x, position.y) <= size) {
          nodeIds.push(nodeId);
        }
      }
      // handle node click when our selector doesn't cover it's center
      // (e.g. large nodes)
      const nodeAtPosition = sigma.getNodeAtPosition(e);
      if((nodeAtPosition && (nodeIds.indexOf(nodeAtPosition) == -1))) {
        nodeIds.push(nodeAtPosition);
      }
      sigma.emit('clickNodes', {
        nodeIds: nodeIds
        //captor: e.data
      })
      _clickPositionX = null;
      _clickPositionY = null;

      return false;
    }

    bindAll();
  }

  mouseSelector();

  // sigma.on('clickNode', (e) => {
  //   console.log('clickNode', e);
  // })
}

//sigmaMouseSelector(sigma);


function drawLabel(
  context,
  data,
  settings
) {
  if (!data.label) return;

  const size = data.size, //settings.labelSize,
    font = settings.labelFont,
    weight = settings.labelWeight,
    color = settings.labelColor.attribute
      ? data[settings.labelColor.attribute] || settings.labelColor.color || "#000"
      : settings.labelColor.color;

  context.fillStyle = color;
  context.font = `${weight} ${size}px ${font}`;

  context.fillText(data.label, data.x, data.y + size / 3);
}


function _sigma(left, right, el, opts) {
  try {
    let graph = new Graph();
    const settings = {
      labelRenderer: drawLabel,
      nodeProgramClasses: {
        circle: CircleNodeProgram.default,  // TODO why default? It seems that import should be fixed
        ccircle: ContourCircleNodeProgram,
        triangle: TriangleNodeProgram,
        ctriangle: ContourTriangleNodeProgram,
        square: SquareNodeProgram,
        csquare: ContourSquareNodeProgram
      },
      ...opts.settings
    };
    let s = new sigma(graph, el, settings);
    console.log('[_sigma] initializing sigma with el', el, 'opts', opts.settings, 'sigma', s);
    console.log('[_sigma] labelRenderedSizeThreshold', opts.settings.labelRenderedSizeThreshold);
    sigmaMouseSelector(s);
    return right(s);
  } catch(e) {
    console.log('[_sigma] error', e);
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
  return takeScreenshot(sigma);
}

function _proxySetSettings(window, sigma, settings) {
  var id = sigma.id;

  window.sigma.instances(id).settings(settings);
  window.sigma.instances(id).refresh();
}

let dummy = function() {};

let _setSettings = function(g, settings) {
  for(const key in settings) {
    //console.log('[setSettings] key', key, settings[key]);
    g.setSetting(key, settings[key]);
  }
}

let _refresh = function(g) {
  return g.refresh();
}

export { _sigma,
         _addRenderer,
         dummy as _bindMouseSelectorPlugin,
         _on,
         _takeScreenshot,
         _proxySetSettings,
         _setSettings,
         _refresh };
