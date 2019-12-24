var Main = require("../output/Main")
var runHalogenAff = require("../output/Halogen.Aff.Util").runHalogenAff
runHalogenAff(Main.run({})("#example"))()
