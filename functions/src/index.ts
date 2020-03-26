import * as functions from 'firebase-functions';
import * as admin from 'firebase-admin';

admin.initializeApp(functions.config().firestore);

exports.onUserCreated = functions.auth.user().onCreate((user) => {
  return admin.firestore().collection("projects").doc(`${user.uid}Starter`).set({ userId: user.uid, name: "emptyStarter" });
});
