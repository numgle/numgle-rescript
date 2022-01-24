// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");

function isInRange(codePoint, range) {
  if (range.start <= codePoint) {
    return codePoint <= range.end;
  } else {
    return false;
  }
}

var charConversionMap = JSON.parse(Fs.readFileSync("./dataset/src/data.json").toString());

exports.isInRange = isInRange;
exports.charConversionMap = charConversionMap;
/* charConversionMap Not a pure module */