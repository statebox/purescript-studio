"use strict";

const firebase = require('firebase')

exports.buildFieldPath = function (fieldNames) {
  return new firebase.firestore.FieldPath(...fieldNames)
}

exports.fieldNames = function (fieldPath) {
  return fieldPath._internalPath.segments
}
