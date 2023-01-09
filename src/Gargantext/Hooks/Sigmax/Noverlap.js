'use strict';

// https://graphology.github.io/standard-library/layout-noverlap.htlm

import NoverlapLayout from 'graphology-layout-noverlap/worker';

export function _init(graph, options) {
  return new NoverlapLayout(graph, {
    maxIterations: 50000,
    settings: {
      gridSize: 5,
      margin: 500,
      expansion: 2.0,
      ratio: 1.5
    }
  });
}

export function _start(layout) {
  console.log('[noverlap] _start', layout);
  return layout.start();
}

export function _stop(layout) {
  console.log('[noverlap] _stop', layout);
  return layout.stop();
}

export function _kill(layout) {
  return layout.kill();
}

export function _isRunning(layout) {
  return layout.isRunning();
}
