%{
    open Puzzle
%}

%token Eof
%token NewLine
%token <string> Rule
%token PuzzleSeparator
%token <string> PuzzleLine
%start <rawPuzzle list> parse

%%

let parse :=
| NewLine*; p=puzzle*; Eof;
    { p }

let puzzle :=
| PuzzleSeparator; NewLine*; rules=rule*; firstLine=PuzzleLine; lines=line+;
    { { rules; lines=firstLine::lines } }

let rule :=
| ~=Rule; NewLine*; <>

let line :=
| ~=PuzzleLine; <>
| NewLine; { "" }
