{
    open RawParser
}

let commentChar = '#'
let ruleChar = '$'
let puzzleChar = '%'
let blank = [' ' '\t']
let symbolChar = [^'$' '#' '%' '\n' '\r']
let newLine = '\n' | '\r' | "\r\n"
let noNewLineChar = [^'\n' '\r']

rule lex = parse
| commentChar noNewLineChar* (newLine | eof)
    { lex lexbuf }
| ruleChar (noNewLineChar* as r) (newLine | eof)
    { Rule r }
| puzzleChar blank* (noNewLineChar+ as id) blank* (newLine | eof)
    { PuzzleSeparator id }
| newLine
    { NewLine }
| (symbolChar+ as l) (newLine | eof)
    { PuzzleLine l }
| eof
    { Eof }
