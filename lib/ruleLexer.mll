{
    open RuleParser
}

let identifier = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z']+
let integer = ['0'-'9']
let whitespace = [' ' '\t']

rule lex = parse
| ',' { Comma }
| ':' { Colon }
| '=' { Equal }
| whitespace { lex lexbuf }
| identifier as id { Identifier id }
| integer as i { Int (int_of_char i) }
| _ as c { Character c }
| eof { Eof }