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
 * Convert null, undefined, or some non-Array value x by an Array.
 * If the value is already an Array, we return it unaltered.
 */
function coerceToArray (x) {
  return !x ? [] : (!Array.isArray(x) ? [x] : x)
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
  delete rawGspn["#comment"]

  rawGspn.project.gspn = coerceToArray(rawGspn.project.gspn)

  rawGspn.project.gspn.forEach(function (gspn) {

    gspn.nodes.place = coerceToArray(gspn.nodes.place)

    gspn.nodes.place.forEach(function (place) {
      place.marking = parseInt(place.marking, 10) || 0
      place.domain  = place.domain || ""
    })

    gspn.nodes.transition = coerceToArray(gspn.nodes.transition)

    gspn.nodes.transition.forEach(function (transition) {
      transition.name = transition.name
      transition.type = transition.type
    })

    gspn.nodes.textBox = coerceToArray(gspn.nodes["text-box"])
    delete gspn.nodes["text-box"]

    gspn.nodes.textBox.forEach(function (textBox) {
      textBox.name        = textBox["name"]
      textBox.text        = textBox["#text"]
      textBox.bold        = textBox["bold"]
      textBox.borderColor = textBox["border-color"]
      textBox.fillColor   = textBox["fill-color"]
      textBox.shadow      = textBox["shadow"]
      textBox.shape       = textBox["shape"]
      textBox.textColor   = textBox["text-color"]
    })

    gspn.edges.arc = coerceToArray(gspn.edges.arc)

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
  const gspn = parseXml(xmlStr)
  console.log('gspn unprocessed =', gspn)
  normalizeGspn_MUTATE_IN_PLACE(gspn)
  console.log('gspn post-processed =', gspn)
  return gspn
}

/** :: String -> Effect PNPRO.Document */
function fromString (xmlStr) {
  return function () {
    return fromStringUnsafe(xmlStr)
  }
}

exports.fromStringUnsafe = fromStringUnsafe

exports.fromString = fromString
