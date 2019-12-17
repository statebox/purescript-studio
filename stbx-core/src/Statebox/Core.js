// module Statebox.Core

// this module assumes that Stbx (from stbx-js) is in the global scope, say from index.html

function decode (hexStr) {
  return function () {
    var x = Stbx.decode(hexStr);
    return x;
  }
}

function decodeEither (hexStr) {
  return function (onError) {
    return function (handler) {
      try {
        var decoded = decode(hexStr)();
        return handler(decoded);
      } catch (error) {
        return onError(error);
      }
    }
  }
}

function stbxObjToJsonString (stbxObj) {
  return JSON.stringify(stbxObj)
}

function hash (hexStr) {
  return Stbx.hash(hexStr);
}

exports.decode = decode;
exports.decodeEither = decodeEither;
exports.stbxObjToJsonString = stbxObjToJsonString;
exports.hash = hash;
