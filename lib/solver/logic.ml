type path_kind = NoPath | Player | Symmetric
type path_index = int
type zone = int
type path = path_kind * path_index
type cell = Defs.Coord.t
type symbol = |

type _ ty =
  | KindTy : path_kind ty
  | IntTy : int ty
  | PathTy : path ty
  | SymbolTy : symbol ty

type _ var =
  | BoolVariable : string -> bool var
  | PathVariable : string -> path var
  | ZoneVariable : string -> zone var
  | CellVariable : string -> cell var

(* Quantifier types *)
type _ quty = ..

type _ expr =
  (* Path kinds *)
  | NoPath : path_kind expr
  | Player : path_kind expr
  | Symmetric : path_kind expr
  (* Index & Int expressions *)
  | Int : int -> int expr
  | Add : int expr list -> int expr
  | Sub : int expr * int expr -> int expr
  | Less : int expr * int expr -> bool expr
  (* Path expressions *)
  | Path : path_kind expr * path_index expr -> path expr
  | KindOf : path expr -> path_kind expr
  | IndexOf : path expr -> path_index expr
  | Var : path var -> path expr
  (* Cells & cell zone expressions *)
  | Zone : zone var -> int expr
  (* Other expressions *)
  | IfThenElse : bool expr * 'a expr * 'a expr -> 'a expr
  (* Boolean formulas *)
  | False : bool expr
  | True : bool expr
  | Bool : bool var -> bool expr
  | NotEqual : 'a ty * 'a expr * 'a expr -> bool expr
  | Equal : 'a ty * 'a expr * 'a expr -> bool expr
  | And : bool expr list -> bool expr
  | Or : bool expr list -> bool expr
  | Xor : bool expr * bool expr -> bool expr
  | Imply : bool expr * bool expr -> bool expr
  | Equiv : bool expr * bool expr -> bool expr
  | Not : bool expr -> bool expr
  (* Symbols expressions *)
  | LinkedSymbolOf : Defs.Coord.t -> symbol expr
  | SymbolOf : Defs.Coord.t -> symbol expr
  (* Relations *)
  | Neighbor : cell * cell -> bool expr
  | Connected : cell * cell -> bool expr
  (* Quantifiers *)
  | Exists : 'a quty * 'a list * ('a -> 'a list -> bool expr) -> bool expr
  | Forall : 'a quty * 'a list * ('a -> 'a list -> bool expr) -> bool expr

let ( <=> ) l r = Equiv (l, r)
let ( ==> ) l r = Imply (l, r)
let ( &&& ) l r = And [ l; r ]
let ( ||| ) l r = Or [ l; r ]
let ( === ) l r ty = Equal (ty, l, r)
let ( =!= ) l r ty = NotEqual (ty, l, r)
let ( +++ ) l r = Add [ l; r ]
let ( --- ) l r = Sub (l, r)
let ( <<< ) l r = Less (l, r)
let ( >>> ) l r = Less (r, l)
let ( ++ ) assertions assertion = List.cons assertion assertions

let ( ++? ) assertions = function
  | Some assertion -> assertions ++ assertion
  | None -> assertions

let evaluate_quantifier (type a) (list : a list) (f : a -> a list -> 'b) =
  let rec iter acc prev = function
    | [] -> acc
    | x :: l ->
        let y = f x (prev @ l) in
        iter (y :: acc) (x :: prev) l
  in
  iter [] [] list

open Format

let pp_sep s fmt () = fprintf fmt "@ %s@ " s

let rec pp_kind fmt = function
  | NoPath -> fprintf fmt "NoPath"
  | Player -> fprintf fmt "Player"
  | Symmetric -> fprintf fmt "Symmetric"
  | KindOf expr -> fprintf fmt "KindOf@ @[<hov 2>(%a)@]" pp_path expr
  | IfThenElse (cond, e, e') ->
      fprintf fmt "@[<hov 0>If@ %a@;Then@ %a@;Else@ %a@]" pp cond pp_kind e
        pp_kind e'

and pp_path fmt = function
  | Path (kind, index) ->
      fprintf fmt "Path@ (%a,@,%a)" pp_kind kind pp_int index
  | Var (PathVariable var) -> fprintf fmt "Var %s" var
  | IfThenElse (cond, e, e') ->
      fprintf fmt "@[<hov 0>If@ %a@;Then@ %a@;Else@ %a@]" pp cond pp_path e
        pp_path e'

and pp_int fmt = function
  | Int i -> fprintf fmt "%i" i
  | Add exprs ->
      fprintf fmt "%a" (pp_print_list ~pp_sep:(pp_sep "+") pp_int) exprs
  | Sub (l, r) -> fprintf fmt "%a - %a" pp_int l pp_int r
  | IndexOf expr -> fprintf fmt "IndexOf@ @[<hov 2>(%a)@]" pp_path expr
  | Zone (ZoneVariable var) -> fprintf fmt "Zone %s" var
  | IfThenElse (cond, e, e') ->
      fprintf fmt "@[<hov 0>If@ %a@;Then@ %a@;Else@ %a@]" pp cond pp_int e
        pp_int e'

and pp_symbol fmt = function
  | LinkedSymbolOf triad ->
      fprintf fmt "LinkedSymbolOf by (%a)" Defs.Coord.pp triad
  | SymbolOf pos -> fprintf fmt "SymbolOf (%a)" Defs.Coord.pp pos
  | IfThenElse (cond, e, e') ->
      fprintf fmt "@[<hov 0>If@ %a@;Then@ %a@;Else@ %a@]" pp cond pp_symbol e
        pp_symbol e'

and pp fmt = function
  | Less (l, r) -> fprintf fmt "%a < %a" pp_int l pp_int r
  | IfThenElse (cond, e, e') ->
      fprintf fmt "@[<hov 0>If@ %a@;Then@ %a@;Else@ %a@]" pp cond pp e pp e'
  | False -> fprintf fmt "false"
  | True -> fprintf fmt "true"
  | Bool (BoolVariable var) -> fprintf fmt "Bool %s" var
  | NotEqual (IntTy, l, r) -> fprintf fmt "%a != %a" pp_int l pp_int r
  | NotEqual (KindTy, l, r) -> fprintf fmt "%a != %a" pp_kind l pp_kind r
  | NotEqual (PathTy, l, r) -> fprintf fmt "%a != %a" pp_path l pp_path r
  | NotEqual (SymbolTy, l, r) -> fprintf fmt "%a != %a" pp_symbol l pp_symbol r
  | Equal (IntTy, l, r) -> fprintf fmt "%a == %a" pp_int l pp_int r
  | Equal (KindTy, l, r) -> fprintf fmt "%a == %a" pp_kind l pp_kind r
  | Equal (PathTy, l, r) -> fprintf fmt "%a == %a" pp_path l pp_path r
  | Equal (SymbolTy, l, r) -> fprintf fmt "%a == %a" pp_symbol l pp_symbol r
  | And exprs ->
      fprintf fmt "%a" (pp_print_list ~pp_sep:(pp_sep "/\\") pp) exprs
  | Or exprs -> fprintf fmt "%a" (pp_print_list ~pp_sep:(pp_sep "\\/") pp) exprs
  | Xor (l, r) -> fprintf fmt "%a XOR %a" pp l pp r
  | Imply (l, r) -> fprintf fmt "%a ==> %a" pp l pp r
  | Equiv (l, r) -> fprintf fmt "%a <=> %a" pp l pp r
  | Not e -> fprintf fmt "Not (%a)" pp e
  | Exists (_, domain, f) ->
      fprintf fmt "Exists @[<v 2>(%a)@]" (pp_print_list pp)
        (evaluate_quantifier domain f)
  | Forall (_, domain, f) ->
      fprintf fmt "Forall @[<v 2>(%a)@]" (pp_print_list pp)
        (evaluate_quantifier domain f)
  | Neighbor (cell, cell') ->
      fprintf fmt "Neighbor (%a, %a)" Defs.Coord.pp cell Defs.Coord.pp cell'
  | Connected (cell, cell') ->
      fprintf fmt "Neighbor (%a, %a)" Defs.Coord.pp cell Defs.Coord.pp cell'
