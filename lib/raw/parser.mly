%{
    open ParserTypes
%}

%token Eof
%token NewLine
%token <string> Rule
%token <string> PuzzleSeparator
%token <string> PuzzleLine
%start <ParserTypes.raw_file> parse

%%

let parse :=
| NewLine*; rules=rule*; p=puzzle*; Eof;
    { rules, p }

let puzzle :=
| name=PuzzleSeparator; NewLine*; rules=rule*; firstLine=PuzzleLine; lines=line+;
    { { name; rules; lines= firstLine::lines } }

let rule :=
| ~=Rule; NewLine*; <>

let line :=
| ~=PuzzleLine; <>
| NewLine; { "" }
