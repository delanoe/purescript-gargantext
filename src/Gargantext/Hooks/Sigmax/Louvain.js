'use strict';

// https://graphology.github.io/standard-library/communities-louvain

import louvain from 'graphology-communities-louvain';

export function _assign(graph, options) {
  louvain.assign(graph, {
    getEdgeWeight: 'weight',
    resolution: 0.8
  });
  return graph;
}
