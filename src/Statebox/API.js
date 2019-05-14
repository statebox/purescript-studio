// module Statebox.API

// this module assumes that stbx.js has been loaded globally, say from index.html

function decode (hexStr) {
  return function () {
    var x = Stbx.decode(hexStr);
    console.log('Statebox.API.js:', JSON.stringify(x));
    return x;
  }
}

exports.decode = decode;
