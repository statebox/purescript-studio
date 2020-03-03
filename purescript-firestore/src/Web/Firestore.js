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
