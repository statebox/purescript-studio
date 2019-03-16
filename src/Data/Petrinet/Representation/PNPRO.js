// module PNPRO

// CAUTION! This assumes that fast-xml-parser is globally available.
const fastXmlParser = window.parser

function parseXml (xmlStr) {
  var options = {
    ignoreAttributes: false,
    attributeNamePrefix: "",
    attrNodeName: false,
    parseAttributeValue: true,
    allowBooleanAttributes: false,
    textNodeName: "#text",
    parseNodeValue: false,
    parseTrueNumberOnly: false,
    trimValues: true,
    ignoreNameSpace: false,
    cdataTagName: false,
    cdataPositionChar: "\\c",
    localeRange: "" // to support non-English characters in tag/attribute values
    // attrValueProcessor: a => a,
    // tagValueProcessor: a => a
  }
  var gspn = fastXmlParser.parse(xmlStr, options)
  return gspn
}

/**
 * Post-process raw converted data from the GSPN XML.
 *
 * !!! This mutates the data structure IN PLACE !!!
 *
 * - Set explicit default values, such as tokens = 0.
 * - Convert null or singleton elements into arrays where needed.
 * - Add some fields, such as 'arc.isPost'.
 * - Rename fields, removing the '-'.
 */
function normalizeGspn_MUTATE_IN_PLACE (rawGspn) {
  console.log('gspn unprocessed =', rawGspn)

  delete rawGspn["#comment"]

  // the XML parser inserts an individual record instead of a singleton Array of records, but we always want an Array
  if (!Array.isArray(rawGspn.project.gspn)) {
    rawGspn.project.gspn = [rawGspn.project.gspn]
  }

  rawGspn.project.gspn.forEach(function (gspn) {

    // the XML parser inserts an individual record instead of a singleton Array of records, but we always want an Array
    if (!Array.isArray(gspn.nodes.place)) {
      gspn.nodes.place = [gspn.nodes.place]
    }

    gspn.nodes.place.forEach(function (place) {
      place.marking = parseInt(place.marking, 10) || 0
      place.domain  = place.domain || ""
    })

    // the XML parser inserts an individual record instead of a singleton Array of records, but we always want an Array
    if (!Array.isArray(gspn.nodes.transition)) {
      gspn.nodes.transition = [gspn.nodes.transition]
    }

    gspn.nodes.transition.forEach(function (transition) {
      transition.name = transition.name
      transition.type = transition.type
    })

    // the XML parser inserts an individual record instead of a singleton Array of records, but we always want an Array
    if (!gspn.nodes["text-box"]) {
      gspn.nodes["text-box"] = []
    } else {
      if (!Array.isArray(gspn.nodes["text-box"])) {
        gspn.nodes["text-box"] = [gspn.nodes["text-box"]]
      }
    }
    gspn.nodes.textBox = gspn.nodes["text-box"]
    delete gspn.nodes["text-box"]

    gspn.nodes.textBox.forEach(function (textBox) {
      textBox.name        = textBox["name"]
      textBox.bold        = textBox["bold"]
      textBox.borderColor = textBox["border-color"]
      textBox.fillColor   = textBox["fill-color"]
      textBox.shadow      = textBox["shadow"]
      textBox.shape       = textBox["shape"]
      textBox.textColor   = textBox["text-color"]
    })

    // the XML parser inserts an individual record instead of a singleton Array of records, but we always want an Array
    if (!Array.isArray(gspn.edges.arc)) {
      gspn.edges.arc = [gspn.edges.arc]
    }

    gspn.edges.arc.forEach(function(arc) {
      // TODO is this correct, or are there more members that "INPUT" and "OUTPUT"?
      arc.isPost = arc.kind == "OUTPUT" // "INPUT" or other values give false
      arc.mult = parseInt(arc.mult, 10) || 1 // default weight is 1
    })

    // TODO handle <points> nested inside arcs:
    // <arc head="foo" kind="INPUT" mult-k="0.5877929687500001" tail="bar">
    //   <point x="89.0" y="26.5"/>
    // </arc>
  })
}

function fromStringUnsafe (xmlStr) {
  const gspnRaw = parseXml(xmlStr)
  normalizeGspn_MUTATE_IN_PLACE(gspnRaw)
  console.log('gspn post-processed =', gspnRaw)
  return gspnRaw
}

exports.fromStringUnsafe = fromStringUnsafe
