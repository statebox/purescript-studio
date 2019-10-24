var introNet = {
  "title":"Arcs",
  "net":{
    "places":[{"label":"Storage","x":20,"y":10},{"label":"CustomerA","x":40,"y":0},{"label":"CustomerB","x":40,"y":20}],
    "transitions":[
      {"label":"Create","x":10,"y":10,"pre":{},"post":{"Storage":1}},
      {"label":"Distribute","x":30,"y":10,"pre":{"Storage":2},"post":{"CustomerA":3,"CustomerB":10}},
      {"label":"ConsumeA","x":50,"y":0,"pre":{"CustomerA":12},"post":{}},
      {"label":"ConsumeB","x":50,"y":20,"pre":{"CustomerB":3},"post":{}}],
    "marking":{}
  }
}

var Main = require("../output/Main")
var runHalogenAff = require("../output/Halogen.Aff.Util").runHalogenAff
runHalogenAff(Main.renderNet("#intro")(introNet))()
Main.main()
