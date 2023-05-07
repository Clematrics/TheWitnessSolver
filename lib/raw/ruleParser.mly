%{
    open Path
    open Defs.Color
    open Defs.Rotation
    open Defs.Symbol
    open RuleType
%}

%token Eos
%token Equal
%token Comma
%token Colon
%token <int> Int
%token <Defs.Property.t> Property
%token <Path.t> Path
%token <Defs.Symbol.t> Symbol
%token <Defs.Shape.t> Shape
%token <Defs.Rotation.t> Rotation
%token <Defs.Color.t> Color
%token <bool> Tag
%token <char> Character
%start <Rule.t> parse

%%

let parse :=
| p=Property; Eos;
    { Property p }
| c=character; Equal; ids=symbols; Eos;
    {
        Assignment (c, fst ids, snd ids)
    }

let character ==
| i=Int; { char_of_int (i + int_of_char '0') }
| ~=Character; <>
| Colon; { ':' }
| Comma; { ',' }
| Equal; { '=' }

let symbols ==
| n=navigation; { Some n, None }
| s=symbol; { None, Some s }
| n=navigation; Comma; s=symbol; { Some n, Some s }
| s=symbol; Comma; n=navigation; { Some n, Some s }

let navigation :=
| ~=Path; <>
| n=Path; Colon; b=Tag;
    {
        match n with
        | Start _ -> Start b
        | Meet _ -> Meet b
        | PathVertical _ -> PathVertical b
        | PathHorizontal _ -> PathHorizontal b
        | _ -> raise (Failure "Cannot use a tag qualifier on navigation symbols other than Start or Paths")
    }
| n=Path; Colon; i=Int;
    {
        match n with
        | End _ -> End i
        | _ -> raise (Failure "Cannot use an int qualifier on navigation symbols other than End")
    }

let symbol :=
| s=Symbol; { s, AnyColor }
| s=Symbol; Colon; c=Color; { s, c }
| s=Symbol; Colon; (sh, r)=shape;
    {
        match s with
        | Shape _ -> Shape (sh, r), AnyColor
        | AntiShape _ -> AntiShape (sh, r), AnyColor
        | _ -> raise (Failure "Cannot use an int qualifier on navigation symbols other than End")
    }
| s=Symbol; Colon; i=Int;
    {
        match s with
        | Triangle _ -> Triangle i, AnyColor
        | _ -> raise (Failure "Cannot use an int qualifier on symbols other than Triangle")
    }

let shape ==
| sh=Shape; { sh, AnyRotation }
| sh=Shape; Colon; r=Rotation; { sh, r }
