// module Statebox.Core

// this module assumes that Stbx (from stbx-js) is in the global scope, say from index.html

function decode (hexStr) {
  return function () {
    var x = Stbx.decode(hexStr);
    return x;
  }
}

function stbxObjToJsonString (stbxObj) {
  return JSON.stringify(stbxObj)
}

function hash (hexStr) {
  return Stbx.hash(hexStr);
}

exports.decode = decode;
exports.stbxObjToJsonString = stbxObjToJsonString;
exports.hash = hash;
