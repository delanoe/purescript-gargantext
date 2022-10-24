'use strict';

import Graph from 'graphology';

export function _newGraph() {
  return new Graph();
}

export function _addNode(g, name, n) {
  return g.addNode(name, n);
}

export function _updateNode(g, name, updater) {
  return g.updateNode(name, updater);
}

export function _mergeNodeAttributes(g, name, attrs) {
  return g.mergeNodeAttributes(name, attrs);
}

export function _addEdge(g, source, target, e) {
  //return g.addEdge(source, target, e);

  // NOTE: Our edge.id is the main key. We don't need sigma
  // auto-generated keys for edges
  return g.addEdgeWithKey(e.id, source, target, e);
}

// Almost the same as graphology.mapNodes but with a change that only
// 1 argument is passed: the whole node structure
export function _mapNodes(g, fn) {
  return g.mapNodes(function(_name, attrs) {
    return fn(attrs);
  });
}

export function _filterNodes(g, fn) {
  return g.filterNodes(function(_name, attrs) {
    return fn(attrs);
  })
}


export function _forEachEdge(g, fn) {
  return g.forEachEdge(function(_name, attrs, _source, _target, _sourceAttributes, _targetAttributes, _undirected) {
    return fn(attrs);
  });
}

export function _updateEachEdgeAttributes(g, fn) {
  return g.updateEachEdgeAttributes(function(_name, attrs, _source, _target) {
    return fn(attrs);
  });
}

// Almost the same as graphology.mapNodes but with a change that only
// 1 argument is passed: the whole node structure
// https://graphology.github.io/iteration.html#mapedges
export function _mapEdges(g, fn) {
  return g.mapEdges(function(_name, attrs, _source, _target, _sourceAttributes, _targetAttributes, _undirected) {
    return fn(attrs);
  });
}

export function _filterEdges(g, fn) {
  return g.filterEdges(function(_name, attrs) {
    return fn(attrs);
  })
}
