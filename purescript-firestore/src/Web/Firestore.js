"use strict";

const firebase = require('firebase')

exports.initializeAppImpl = firebase.initializeApp

exports.firestoreImpl = firebase.firestore

exports.docImpl = firebase.firestore.Firestore.doc

exports.getImpl = firebase.firestore.DocumentSnapshot.get

exports.dataImpl = firebase.firestore.DocumentSnapshot.data
