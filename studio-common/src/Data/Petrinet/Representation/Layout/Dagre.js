// Warning: expects `dagre` in the global scope

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
  g.edges().forEach(function(e, i) {
    input.edges[i].waypoints = g.edge(e).points.slice(1, -1);
  });

  output.edges = input.edges;

  return output;
}
