var Main = require("../output/index.js")

////////////////////////////////////////////////////////////////////////////////
//
// initialise Firebase
//
////////////////////////////////////////////////////////////////////////////////

var firebaseConfig = {
  apiKey: "AIzaSyAhl4uChdRK_yXiYybtXfqG6uUEk1hAB9A",
  authDomain: "statebox-kdmoncat.firebaseapp.com",
  databaseURL: "https://statebox-kdmoncat.firebaseio.com",
  projectId: "statebox-kdmoncat",
  storageBucket: "statebox-kdmoncat.appspot.com",
  messagingSenderId: "455902306352",
  appId: "1:455902306352:web:6fcdfeb29f583d118d0df5",
  measurementId: "G-9FF747MDHW"
}

let firebase = window.firebase

firebase.initializeApp(firebaseConfig)
firebase.analytics()
var db = firebase.firestore()

firebase.auth().setPersistence(firebase.auth.Auth.Persistence.LOCAL)

var ui = new firebaseui.auth.AuthUI(firebase.auth())
var uiConfig = {
  credentialHelper: firebaseui.auth.CredentialHelper.NONE,
  signInFlow: 'popup', // use popup for IDP Providers sign-in flow instead of the default, redirect
  signInOptions: [
    firebase.auth.EmailAuthProvider.PROVIDER_ID,
  ],
}

var loggedIn = false
firebase.auth().onAuthStateChanged(function (user) {
  if (user) {
    start(user)
    loggedIn = true
  } else {
    console.log("firebase auth: not logged in.")
    if (!loggedIn) {
      ui.start('#firebaseui-auth-container', uiConfig)
    } else {
      location.reload()
    }
  }
})

function start (user) {
  console.log('user =', user)
  document.getElementById('email').innerText = user && user.email || ""
  document.getElementById('firebaseui-auth-container').style.display = 'none'

  console.log("firebase auth: logged in.")

  Main.main()
  document.getElementById('sign-out').onclick = function () {
    firebase.auth().signOut()
  }
}
