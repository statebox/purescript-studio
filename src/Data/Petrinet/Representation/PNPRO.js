// module PNPRO

var NODE_TYPE_ELEMENT = 1,
    NODE_TYPE_TEXT = 3;

/**
 * @param ATTRS_KEY a spearate subrecord to put the attrs in, eg "attrs", or null to
 * put the on the same level as the children. Depending on your XML, this may
 * produce clases if a note and attritbute share the same name.
 */
function xmlToJson_(ATTRS_KEY, SKIP_TEXT_NODE) {
  return function xmlToJson(xml) {
    var result = {}
    if (xml.nodeType == NODE_TYPE_ELEMENT) {
      // do attributes
      if (xml.attributes.length > 0) {
        if (ATTRS_KEY) {
          result[ATTRS_KEY] = {};
        }
        for (var j = 0; j < xml.attributes.length; j++) {
          var attribute = xml.attributes.item(j);
          if (ATTRS_KEY) {
            result[ATTRS_KEY][attribute.nodeName] = attribute.nodeValue
          } else {
            result[attribute.nodeName] = attribute.nodeValue
          }
        }
      }
    } // TODO SKIP_TEXT_NODE?
    else if (xml.nodeType == NODE_TYPE_TEXT) {
      result = xml.nodeValue
    }

    // visit children
    // if just one text node inside
    // TODO SKIP_TEXT_NODE?
    if (xml.hasChildNodes() && xml.childNodes.length === 1 && xml.childNodes[0].nodeType === NODE_TYPE_TEXT) {
      result = xml.childNodes[0].nodeValue;
    } else if (xml.hasChildNodes()) {
      for (var i = 0; i < xml.childNodes.length; i++) {
        var item = xml.childNodes.item(i);
        var nodeName = item.nodeName;
        if (//!SKIP_TEXT_NODE ||
          xml.childNodes[i].nodeType != NODE_TYPE_TEXT) { // TODO GSPN: suppress text nodes?
          if (typeof(result[nodeName]) == "undefined"
              //&& xml.childNodes[i].nodeType != 3 // TODO GSPN: suppress text nodes?
             ) { // insert json result as a single value instead of an array
            result[nodeName] = xmlToJson(item);
          } else { // insert json result as an aray element
            if (typeof(result[nodeName].push) == "undefined") { // is this an isArray check on the value at key nodeName?
              var old = result[nodeName];
              result[nodeName] = [];
              result[nodeName].push(old);
            }
            result[nodeName].push(xmlToJson(item));
          }
        }
      }
    }
    return result;
  }
}

////////////////////////////////////////////////////////////////////////////////

/**
 * Post-process GSPN XML:
 *
 * - Parse floats
 * - ...
 *
 * TODO Needs a bit more cleanup.
 */
function mutateGSPNJson1 (rawGspn) {
  console.log(rawGspn)

  delete rawGspn["#comment"]

  // the XML parser inserts an individual record instead of a singleton Array of records, but we always want an Array
  if (!Array.isArray(rawGspn.project.gspn)) {
    rawGspn.project.gspn = [rawGspn.project.gspn];
  }


  rawGspn.project.gspn.forEach(function (gspn) {

    // the XML parser inserts an individual record instead of a singleton Array of records, but we always want an Array
    if (!Array.isArray(gspn.nodes.place)) {
      gspn.nodes.place = [gspn.nodes.place];
    }

    gspn.nodes.place.forEach(function (place) {
      place.x       = parseFloat(place.x)
      place.y       = parseFloat(place.y)
      place.marking = parseInt(place.marking, 10) || 0
      place.domain  = place.domain || ""
    })

    // the XML parser inserts an individual record instead of a singleton Array of records, but we always want an Array
    if (!Array.isArray(gspn.nodes.transition)) {
      gspn.nodes.transition = [gspn.nodes.transition];
    }

    gspn.nodes.transition.forEach(function (transition) {
      transition.name = transition.name
      transition.type = transition.type
      transition.x    = parseFloat(transition.x)
      transition.y    = parseFloat(transition.y)
    })

    // the XML parser inserts an individual record instead of a singleton Array of records, but we always want an Array
    if (!Array.isArray(gspn.nodes["text-box"]) {
      gspn.nodes["text-box"] = [gspn.nodes["text-box"]];
    }

    gspn.nodes["text-box"].forEach(function (textBox) {
      textBox.name        = textBox.name
      textBox.x           = parseFloat(textBox.x)
      textBox.y           = parseFloat(textBox.y)
      textBox.height      = parseFloat(textBox.height)
      textBox.width       = parseFloat(textBox.width)
      textBox.bold        = textBox.bold
      textBox.borderColor = textBox["border-color"]
      textBox.fillColor   = textBox["fill-color"]
      textBox.shadow      = textBox.shadow
      textBox.shape       = textBox.shape
      textBox.textColor   = textBox["text-color"]
    })

    // the XML parser inserts an individual record instead of a singleton Array of records, but we always want an Array
    if (!Array.isArray(gspn.edges.arc)) {
      gspn.edges.arc = [gspn.edges.arc];
    }

    gspn.edges.arc.forEach(function(arc) {
      // TODO is this correct, or are there more members that "INPUT" and "OUTPUT"?
      arc.isPost = arc.kind == "OUTPUT" // "INPUT" or other values give false
    })

    // TODO handle <points> nested inside arcs:
    // <arc head="foo" kind="INPUT" mult-k="0.5877929687500001" tail="bar">
    //   <point x="89.0" y="26.5"/>
    // </arc>
  })
}

function fromGSPNJson1 (xmlDom) {
  var rawGspn = xmlToJson_(null, true)(xmlDom)
  mutateGSPNJson1(rawGspn)
  return rawGspn
}

////////////////////////////////////////////////////////////////////////////////

function parseXml (xmlStr) {
  const parser = new DOMParser();
  const xmlDoc = parser.parseFromString(xmlStr, "application/xml");
  return xmlDoc;
}

function fromStringUnsafe (xmlStr) {
  const xmlDoc = parseXml(xmlStr)
  const gspnRaw = fromGSPNJson1(xmlDoc)
console.log('gspnRaw =', gspnRaw)
  return gspnRaw
}

exports.fromStringUnsafe = fromStringUnsafe
