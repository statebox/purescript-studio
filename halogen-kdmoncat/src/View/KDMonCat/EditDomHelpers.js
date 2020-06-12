"use strict";

exports.copyToClipboard = function (text) {
  var textArea = document.createElement("textarea");
  textArea.value = text;
  document.body.appendChild(textArea);
  textArea.focus();
  textArea.select();
  document.execCommand('copy');
  document.body.removeChild(textArea);
}

exports.insertText = function (text) {
  document.execCommand("insertText", false, text);
}
