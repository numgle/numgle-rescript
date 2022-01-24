open NodeJs

type charRange = {
  start: int,
  end: int
}

let isInRange = (codePoint: int, range: charRange) => {
  range.start <= codePoint && codePoint <= range.end
}

type charRanges = {
  completeHangul: charRange,
  notCompleteHangul: charRange,
  uppercase: charRange,
  lowercase: charRange,
  number: charRange,
  special: array<int>,
}

type charConversionMapType = {
  cho: array<string>,
  jong: array<string>,
  jung: array<string>,
  cj: array<array<string>>,
  han: array<string>,
  englishUpper: array<string>,
  englishLower: array<string>,
  number: array<string>,
  special: array<string>,
  range: charRanges,
}

@scope("JSON") @val
external parseIntoCharConversionMap: string => charConversionMapType = "parse"

let charConversionMap = Fs.readFileSync("./dataset/src/data.json")->Buffer.toString->parseIntoCharConversionMap