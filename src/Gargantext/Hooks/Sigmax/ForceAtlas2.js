'use strict';

// https://graphology.github.io/standard-library/layout-forceatlas2.html

import FA2Layout from 'graphology-layout-forceatlas2/worker';

export function _init(graph) {
  return new FA2Layout(graph, {
    settings: {gravity: 1},
    getEdgeWeight: 'weight'
  })
}

export function _start(layout) {
  return layout.start();
}

export function _stop(layout) {
  return layout.stop();
}

export function _kill(layout) {
  return layout.kill();
}

export function _isRunning(layout) {
  return layout.isRunning();
}
