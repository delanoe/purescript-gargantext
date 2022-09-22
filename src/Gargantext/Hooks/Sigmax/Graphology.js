'use strict';

import Graph from 'graphology';

export function _newGraph() {
  return new Graph();
}

export function _addNode(g, name, n) {
  return g.addNode(name, n);
}

export function _addEdge(g, source, target, e) {
  return g.addEdge(source, target, e);
}

// Almost the same as graphology.mapNodes but with a change that only
// 1 argument is passed: the whole node structure
export function _mapNodes(g, fn) {
  return g.mapNodes(function(name, attrs) {
    return fn({id: name, ...attrs});
  });
}

// Almost the same as graphology.mapNodes but with a change that only
// 1 argument is passed: the whole node structure
// TODO Add source, target
export function _mapEdges(g, fn) {
  return g.mapEdges(function(name, attrs) {
    return fn({id: name, ...attrs});
  });
}