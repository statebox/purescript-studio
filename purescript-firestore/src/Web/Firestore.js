"use strict";

const firebase = require('firebase')

exports.initializeAppImpl = function (options, name) {
  // optional arguments should be passed as `undefined` and not as `null`
  name = name === null ? undefined : name

  return firebase.initializeApp(options, name)
}

exports.firestoreImpl = function (app) {
  return app.firestore()
}

exports.docImpl = function (firestore, documentPath) {
  return firestore.doc(documentPath)
}

// TODO: such an approach looks very brittle
const wrapSetOptionsInFieldPath = function (setOptions) {
  if (setOptions.mergeFields !== undefined) {
    const mergeFields = setOptions.mergeFields.map(function (element) {
      if (Array.isArray(element)) {
        element = new firebase.firestore.FieldPath(...element)
      }

      return element
    })

    setOptions.mergeFields = mergeFields
  }

  return setOptions
}

exports.setImpl = function (documentReference, data, setOptions) {
  // optional arguments should be passed as `undefined` and not as `null`
  setOptions = setOptions === null ? undefined : wrapSetOptionsInFieldPath(setOptions)

  return documentReference.set(data, setOptions)
}

exports.getImpl = function (documentReference, getOptions) {
  // optional arguments should be passed as `undefined` and not as `null`
  getOptions = getOptions === null ? undefined : getOptions

  return documentReference.get(getOptions)
}

exports.dataImpl = function (documentSnapshot, snapshotOptions) {
  // optional arguments should be passed as `undefined` and not as `null`
  snapshotOptions = snapshotOptions === null ? undefined : snapshotOptions

  const ret = documentSnapshot.data(snapshotOptions)

  return ret === undefined ? null : ret
}
