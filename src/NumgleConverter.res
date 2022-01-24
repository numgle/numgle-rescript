open Data
open Belt
let { includes, fromMap, joinWith, indexOf } = module(Js.Array2)
let { fromCodePoint, codePointAt, castToArrayLike } = module(Js.String2)
let { getWithDefault, getExn } = module(Belt.Option)

type token =
  | Empty
  | CompleteHangul(int)
  | NotCompleteHangul(int)
  | EnglishUpper(int)
  | EnglishLower(int)
  | Number(int)
  | SpecialLetter(int)
  | Unknown

type separatedHangulCodes = (int, int, int)

let toToken = (codePoint: int): token => {
  let range = charConversionMap.range
  if %re("/\s/")->Js.Re.test_(codePoint->fromCodePoint) {
    Empty
  } else if codePoint->isInRange(range.completeHangul) {
    CompleteHangul(codePoint)
  } else if codePoint->isInRange(range.notCompleteHangul) {
    NotCompleteHangul(codePoint)
  } else if codePoint->isInRange(range.uppercase) {
    EnglishUpper(codePoint)
  } else if codePoint->isInRange(range.lowercase) {
    EnglishLower(codePoint)
  } else if codePoint->isInRange(range.number) {
    Number(codePoint)
  } else if range.special->includes(codePoint) {
    SpecialLetter(codePoint)
  } else {
    Unknown
  }
}

let separateHangul = (hangulCodePoint: int): separatedHangulCodes => {
  let cho = (hangulCodePoint - 44032) / 28 / 21
  let jung = mod((hangulCodePoint - 44032) / 28, 21)
  let jong = mod((hangulCodePoint - 44032), 28)
  (cho, jung, jong)
}

let isCodePointConvertible = (separated: separatedHangulCodes) => {
  let (cho, jung, jong) = separated
  if jong != 0 && charConversionMap.jong[jong] === Some("") {
    false
  } else if jung >= 8 && jung != 20 {
    charConversionMap.jung[jung - 8] !== Some("")
  } else {
    switch charConversionMap.cj[min(8, jung)] {
      | Some(cj) => cj[cho] !== Some("")
      | None => false
    }
  }
}

let convert = (input: string) => {
  let map = charConversionMap
  input
    ->castToArrayLike
    ->fromMap(str => {
      switch str->codePointAt(0)->getExn->toToken {
        | Unknown
        | Empty => ""
        | CompleteHangul(hangulCodePoint) =>
          let separated = separateHangul(hangulCodePoint)
          let (cho, jung, jong) = separated
          if !isCodePointConvertible(separated) {
            ""
          } else if jung >= 8 && jung != 20 {
            map.jong[jong]->getWithDefault("")
            ++ map.jung[jung - 8]->getWithDefault("")
            ++ map.cho[cho]->getWithDefault("")
          } else {
            let cj = switch map.cj[min(8, jung)] {
              | Some(cj) => cj[cho]->getWithDefault("")
              | None => ""
            }
            map.jong[jong]->getWithDefault("") ++ cj
          }
        | NotCompleteHangul(hangulCodePoint) => map.han[hangulCodePoint - map.range.notCompleteHangul.start]->getWithDefault("")
        | EnglishUpper(codePoint) => map.englishUpper[codePoint - map.range.uppercase.start]->getWithDefault("")
        | EnglishLower(codePoint) => map.englishLower[codePoint - map.range.lowercase.start]->getWithDefault("")
        | Number(codePoint) => map.number[codePoint - map.range.number.start]->getWithDefault("")
        | SpecialLetter(codePoint) => map.special[map.range.special->indexOf(codePoint)]->getWithDefault("")
      }
    })
    ->joinWith("\n")
}