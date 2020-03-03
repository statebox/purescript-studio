"use strict";

const firebase = require('firebase')

exports.initializeAppImpl = firebase.initializeApp

exports.firestoreImpl = firebase.firestore

exports.docImpl = function (firestore, documentPath) {
  return firestore.doc(documentPath)
}

exports.setImpl = function (documentReference, data, setOptions) {
  ret = documentReference.set(data, setOptions)

  console.log('ret', ret)
  return ret
}

exports.getImpl = function (documentReference) {
  return function (getOptions) {
    return documentReference.get(getOptions)
  }
}

exports.dataImpl = function (documentSnapshot) {
  return function (snapshotOptions) {
    return documentSnapshot.data(snapshotOptions)
  }
}
