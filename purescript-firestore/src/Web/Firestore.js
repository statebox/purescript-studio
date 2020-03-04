"use strict";

const firebase = require('firebase')

exports.initializeAppImpl = firebase.initializeApp

exports.firestoreImpl = firebase.firestore

exports.docImpl = function (firestore, documentPath) {
  return firestore.doc(documentPath)
}

exports.setImpl = function (documentReference, data, setOptions) {
  return documentReference.set(data, setOptions)
}

exports.getImpl = function (documentReference, getOptions) {
  // optional arguments should be passed as `undefined` and not as `null`
  getOptions = getOptions === null ? undefined : getOptions

  return documentReference.get(getOptions)
}

exports.dataImpl = function (documentSnapshot, snapshotOptions) {
  const ret = documentSnapshot.data(snapshotOptions)

  return ret === undefined ? null : ret
}
