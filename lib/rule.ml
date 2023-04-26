include RuleType

let from_raw raw =
  let lexBuf = Lexing.from_string raw in
  RuleParser.parse RuleLexer.lex lexBuf
