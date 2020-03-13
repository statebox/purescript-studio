var firebase = require('firebase/app')
require('firebase/auth')
require('firebase/analytics')
var firebaseui = require('firebaseui')

var Main = require("../output/index.js")
var runHalogenAff = require("../output/Halogen.Aff.Util").runHalogenAff
var initialModel = Main.parseHash(location.hash)

var firebaseConfig = {
  apiKey: "AIzaSyAhl4uChdRK_yXiYybtXfqG6uUEk1hAB9A",
  authDomain: "statebox-kdmoncat.firebaseapp.com",
  databaseURL: "https://statebox-kdmoncat.firebaseio.com",
  projectId: "statebox-kdmoncat",
  storageBucket: "statebox-kdmoncat.appspot.com",
  messagingSenderId: "455902306352",
  appId: "1:455902306352:web:09f5eeab367d53bf8d0df5",
  measurementId: "G-Z37CXN9Y8D"
}

firebase.initializeApp(firebaseConfig)
firebase.analytics()

firebase.auth().setPersistence(firebase.auth.Auth.Persistence.LOCAL)

var ui = new firebaseui.auth.AuthUI(firebase.auth())
var uiConfig = {
  credentialHelper: firebaseui.auth.CredentialHelper.NONE,
  // Will use popup for IDP Providers sign-in flow instead of the default, redirect.
  signInFlow: 'popup',
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
    if (!loggedIn)
      ui.start('#firebaseui-auth-container', uiConfig)
    else
      location.reload()
  }
});

function start(user) {
  document.getElementById('email').innerText = user && user.email || ""
  document.getElementById('firebaseui-auth-container').style.display = 'none'
  runHalogenAff(Main.runJs2(initialModel)("body"))()

  document.getElementById('sign-out').onclick = function() {
    firebase.auth().signOut()
  }
}
