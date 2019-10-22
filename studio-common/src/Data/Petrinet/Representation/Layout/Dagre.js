"use strict";

var dagre = require('dagre');

exports.dagreLayout = function (input) {
  const g = new dagre.graphlib.Graph({ directed: true });
  g.setGraph({ rankdir: 'LR' });
  g.setDefaultEdgeLabel(function() { return {}; });

  input.places.forEach(function (id) { g.setNode(id, {}); });
  input.transitions.forEach(function (id) { g.setNode(id, {}); });
  
  input.edges.forEach(function(e) {
    g.setEdge(e.source, e.target);
  })

  dagre.layout(g);

  const output = { places: {}, transitions: {} };
  input.places.forEach(function(id) { 
    const n = g.node(id);
    output.places[id] = { x: n.x, y: n.y };
  });
  input.transitions.forEach(function(id) { 
    const n = g.node(id);
    output.transitions[id] = { x: n.x, y: n.y };
  });

  return output;
}