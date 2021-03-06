// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Data = require("./Data.bs.js");
var Curry = require("rescript/lib/js/curry.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Belt_Option = require("rescript/lib/js/belt_Option.js");

function includes(prim0, prim1) {
  return prim0.includes(prim1);
}

function fromMap(prim0, prim1) {
  return Array.from(prim0, Curry.__1(prim1));
}

function joinWith(prim0, prim1) {
  return prim0.join(prim1);
}

function indexOf(prim0, prim1) {
  return prim0.indexOf(prim1);
}

function fromCodePoint(prim) {
  return String.fromCodePoint(prim);
}

function codePointAt(prim0, prim1) {
  return prim0.codePointAt(prim1);
}

function castToArrayLike(prim) {
  return prim;
}

function toToken(codePoint) {
  var range = Data.charConversionMap.range;
  if (/\s/.test(String.fromCodePoint(codePoint))) {
    return /* Empty */0;
  } else if (Data.isInRange(codePoint, range.completeHangul)) {
    return {
            TAG: /* CompleteHangul */0,
            _0: codePoint
          };
  } else if (Data.isInRange(codePoint, range.notCompleteHangul)) {
    return {
            TAG: /* NotCompleteHangul */1,
            _0: codePoint
          };
  } else if (Data.isInRange(codePoint, range.uppercase)) {
    return {
            TAG: /* EnglishUpper */2,
            _0: codePoint
          };
  } else if (Data.isInRange(codePoint, range.lowercase)) {
    return {
            TAG: /* EnglishLower */3,
            _0: codePoint
          };
  } else if (Data.isInRange(codePoint, range.number)) {
    return {
            TAG: /* Number */4,
            _0: codePoint
          };
  } else if (range.special.includes(codePoint)) {
    return {
            TAG: /* SpecialLetter */5,
            _0: codePoint
          };
  } else {
    return /* Unknown */1;
  }
}

function separateHangul(hangulCodePoint) {
  var cho = ((hangulCodePoint - 44032 | 0) / 28 | 0) / 21 | 0;
  var jung = ((hangulCodePoint - 44032 | 0) / 28 | 0) % 21;
  var jong = (hangulCodePoint - 44032 | 0) % 28;
  return [
          cho,
          jung,
          jong
        ];
}

function isCodePointConvertible(separated) {
  var jong = separated[2];
  var jung = separated[1];
  if (jong !== 0 && Belt_Array.get(Data.charConversionMap.jong, jong) === "") {
    return false;
  }
  if (jung >= 8 && jung !== 20) {
    return Belt_Array.get(Data.charConversionMap.jung, jung - 8 | 0) !== "";
  }
  var cj = Belt_Array.get(Data.charConversionMap.cj, 8 < jung ? 8 : jung);
  if (cj !== undefined) {
    return Belt_Array.get(cj, separated[0]) !== "";
  } else {
    return false;
  }
}

function convert(input) {
  return Array.from(input, (function (str) {
                  var hangulCodePoint = toToken(Belt_Option.getExn(str.codePointAt(0)));
                  if (typeof hangulCodePoint === "number") {
                    return "";
                  }
                  switch (hangulCodePoint.TAG | 0) {
                    case /* CompleteHangul */0 :
                        var separated = separateHangul(hangulCodePoint._0);
                        var jong = separated[2];
                        var jung = separated[1];
                        var cho = separated[0];
                        if (!isCodePointConvertible(separated)) {
                          return "";
                        }
                        if (jung >= 8 && jung !== 20) {
                          return Belt_Option.getWithDefault(Belt_Array.get(Data.charConversionMap.jong, jong), "") + Belt_Option.getWithDefault(Belt_Array.get(Data.charConversionMap.jung, jung - 8 | 0), "") + Belt_Option.getWithDefault(Belt_Array.get(Data.charConversionMap.cho, cho), "");
                        }
                        var cj = Belt_Array.get(Data.charConversionMap.cj, 8 < jung ? 8 : jung);
                        var cj$1 = cj !== undefined ? Belt_Option.getWithDefault(Belt_Array.get(cj, cho), "") : "";
                        return Belt_Option.getWithDefault(Belt_Array.get(Data.charConversionMap.jong, jong), "") + cj$1;
                    case /* NotCompleteHangul */1 :
                        return Belt_Option.getWithDefault(Belt_Array.get(Data.charConversionMap.han, hangulCodePoint._0 - Data.charConversionMap.range.notCompleteHangul.start | 0), "");
                    case /* EnglishUpper */2 :
                        return Belt_Option.getWithDefault(Belt_Array.get(Data.charConversionMap.englishUpper, hangulCodePoint._0 - Data.charConversionMap.range.uppercase.start | 0), "");
                    case /* EnglishLower */3 :
                        return Belt_Option.getWithDefault(Belt_Array.get(Data.charConversionMap.englishLower, hangulCodePoint._0 - Data.charConversionMap.range.lowercase.start | 0), "");
                    case /* Number */4 :
                        return Belt_Option.getWithDefault(Belt_Array.get(Data.charConversionMap.number, hangulCodePoint._0 - Data.charConversionMap.range.number.start | 0), "");
                    case /* SpecialLetter */5 :
                        return Belt_Option.getWithDefault(Belt_Array.get(Data.charConversionMap.special, Data.charConversionMap.range.special.indexOf(hangulCodePoint._0)), "");
                    
                  }
                })).join("\n");
}

var getWithDefault = Belt_Option.getWithDefault;

var getExn = Belt_Option.getExn;

exports.includes = includes;
exports.fromMap = fromMap;
exports.joinWith = joinWith;
exports.indexOf = indexOf;
exports.fromCodePoint = fromCodePoint;
exports.codePointAt = codePointAt;
exports.castToArrayLike = castToArrayLike;
exports.getWithDefault = getWithDefault;
exports.getExn = getExn;
exports.toToken = toToken;
exports.separateHangul = separateHangul;
exports.isCodePointConvertible = isCodePointConvertible;
exports.convert = convert;
/* Data Not a pure module */
