%{
    open Definition
    open RuleType
%}

%token Eos
%token Equal
%token Comma
%token Colon
%token <int> Int
%token <Property.t> Property
%token <Definition.nav> Navigation
%token <Definition.symbol> Symbol
%token <Definition.shape> Shape
%token <Definition.rotation> Rotation
%token <Definition.color> Color
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
| ~=Navigation; <>
| n=Navigation; Colon; b=Tag;
    {
        match n with
        | Start _ -> Start b
        | PathVertical _ -> PathVertical b
        | PathHorizontal _ -> PathHorizontal b
        | _ -> raise (Failure "Cannot use a tag qualifier on navigation symbols other than Start or Paths")
    }
| n=Navigation; Colon; i=Int;
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

let shape ==
| sh=Shape; { sh, AnyRotation }
| sh=Shape; Colon; r=Rotation; { sh, r }
