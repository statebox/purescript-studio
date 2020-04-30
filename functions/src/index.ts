import * as functions from 'firebase-functions';
import * as admin from 'firebase-admin';

admin.initializeApp(functions.config().firestore);
const db = admin.firestore();

exports.onUserCreated = functions.auth.user().onCreate(async (user) => {
});
