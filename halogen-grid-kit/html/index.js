var Main = require("../output/Main")
var runHalogenAff = require("../output/Halogen.Aff.Util").runHalogenAff
function f(form) {
  var spacing = Math.pow(10, form.spacing.value)
  var s = Math.pow(10, form.scale.value);
  var x = form.x.value;
  var y = form.y.value;
  document.getElementById("canvas").innerHTML = "";
  runHalogenAff(Main.run({
    gridSpacing: spacing,
    range: {
      topLeft: { x: (-0.5 - x)*s, y: (-0.5 - y)*s },
      bottomRight: { x: (0.5 - x)*s, y: (0.5 - y)*s }
    },
    size: { x: 600, y: 600 }
  })("#canvas"))()
}
f(document.forms[0])
window.f = f
