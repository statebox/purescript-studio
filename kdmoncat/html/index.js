var api = require('../output/index.js')

document.forms[0].onsubmit = function() {
  try {
    document.forms[0].json.value = api.toJSON({
      pixels: document.forms[0].pixels.value.trim(),
      context: document.forms[0].context.value.trim()
    })()
  } catch (e) {
    alert(e.message)
  }
  return false
}
