include RuleType

let from_raw raw =
  try
    let lexBuf = Lexing.from_string raw in
    RuleParser.parse RuleLexer.lex lexBuf
  with _ -> raise (Failure ("Rule parsing error: " ^ raw))
