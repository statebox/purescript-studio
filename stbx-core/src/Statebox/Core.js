// module Statebox.Core

// this module assumes that stbx.js has been loaded globally, say from index.html

function decode (hexStr) {
  return function () {
    var x = Stbx.decode(hexStr);
    return x;
  }
}

function stbxObjToJsonString (stbxObj) {
  return JSON.stringify(stbxObj)
}

exports.decode = decode;
exports.stbxObjToJsonString = stbxObjToJsonString;
