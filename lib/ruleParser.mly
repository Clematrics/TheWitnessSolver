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
// | id=Identifier; Eos;
//     { Property (find_rule id) }
// | c=character; Equal; id1=identifier; id2=other_identifier?; Eos;
//     {
//         match id1, id2 with
//         | SymbolType s, None -> Assignment (c, None, Some s)
//         | NavType n, None -> Assignment (c, Some n, None)
//         | SymbolType s, Some (NavType n)
//         | NavType n, Some (SymbolType s) -> Assignment (c, Some n, Some s)
//         | NavType _, Some (NavType _) -> raise (Failure "Cannot have two navigation elements in the same assignment")
//         | SymbolType _, Some (SymbolType _) -> raise (Failure "Cannot have two symbols in the same assignment")
//     }

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

// let other_identifier :=
// | Comma; ~=identifier; <>

// let identifier :=
// | id=Identifier; qualifier=qualifier*;
//     {
//         let id = find_id id in
//         match qualifier with
//         | Some (EndIdentifier n) -> begin
//             (* Number only valid if End navigation element *)
//             match id with
//             | NavType (End _) -> NavType (End n)
//             | _ -> raise (Failure "Cannot specify an identifier for navigation elements other than end bits")
//             end
//         | Some (SymbolColor c) -> begin
//             (* Color only valid if symbol *)
//             match id with
//             | SymbolType (s, _) -> SymbolType (s, c)
//             | NavType _ -> raise (Failure "Cannot specify a color for navigation elements other than end bits")
//             end
//         | None -> id
//     }

// let qualifier :=
// | Colon; i=Int;
//     { EndIdentifier i }
// | Colon; id=Identifier;
//     { SymbolColor (find_color id) }
