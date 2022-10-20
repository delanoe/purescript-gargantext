'use strict';

// https://graphology.github.io/standard-library/layout-forceatlas2.html

// import forceAtlas2 from 'graphology-layout-forceatlas2';
import FA2Layout from 'graphology-layout-forceatlas2/worker';

export function _init(graph, settings) {
  // let inferred = forceAtlas2.inferSettings(graph);
  // console.log('[init] graph', graph, 'settings', settings);
  return new FA2Layout(graph, {
    settings,
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
