var Main = require("../output/KDMonCat.Main")
var runHalogenAff = require("../output/Halogen.Aff.Util").runHalogenAff
var initialModel = Main.parseHash(location.hash)
runHalogenAff(Main.runJs2(initialModel)("body"))()
