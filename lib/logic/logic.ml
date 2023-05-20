open Puzzle
open Defs

type path_kind
type path_index = int
type path = path_kind * path_index
type _ ty = KindTy : path_kind ty | IntTy : path_index ty | PathTy : path ty

type _ var =
  | BoolVariable : string -> bool var
  | PathVariable : string -> path var

(* Quantifier types *)
type _ quty = ..
type _ quty += CoordTy : Coord.t quty
type _ quty += BoolVarTy : bool var quty
type _ quty += PathVarTy : path var quty
type _ quty += CoordPathVarTy : (Coord.t * path var) quty

type _ expr =
  (* Path kinds *)
  | NoPath : path_kind expr
  | Player : path_kind expr
  | Symmetric : path_kind expr
  (* Index expressions *)
  | Add : int expr list -> int expr
  | Sub : int expr * int expr -> int expr
  | Int : path_index -> path_index expr
  (* Path expressions *)
  | Path : path_kind expr * path_index expr -> path expr
  | KindOf : path expr -> path_kind expr
  | IndexOf : path expr -> path_index expr
  | Var : path var -> path expr
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

module EdgesVar = Map.Make (Edge)
module PosSet = CoordSet
module PosVar = CoordMap

type context = {
  junctions : path var PosVar.t;
  activated : bool var PosVar.t;
  (* Starts (usable by the player) & Ends positions *)
  starts : PosSet.t;
  ends : PosSet.t;
  (* symbols *)
  hexagons : Color.t PosVar.t;
}

let ( ++ ) assertions assertion = List.cons assertion assertions

let ( ++? ) assertions = function
  | Some assertion -> List.cons assertion assertions
  | None -> assertions

let make_context puzzle =
  let junctions =
    Graph.fold_points
      (fun (x, y) ->
        let name = Printf.sprintf "path_(%i,%i)" x y in
        PosVar.add (x, y) (PathVariable name))
      puzzle.logic_graph PosVar.empty
  and activated =
    CoordMap.fold
      (fun (x, y) _ ->
        let name = Printf.sprintf "symbol_activated(%i,%i)" x y in
        PosVar.add (x, y) (BoolVariable name))
      puzzle.symbols PosVar.empty
  and starts =
    CoordMap.fold
      (fun pos b -> if b then PosSet.add pos else Fun.id)
      puzzle.starts PosSet.empty
  and ends = IntMap.fold (fun _ pos -> PosSet.add pos) puzzle.ends PosSet.empty
  and hexagons =
    puzzle.symbols
    |> CoordMap.filter_map (fun _ -> function
         | Symbol.Hexagon, color -> Some color | _ -> None)
  in
  { junctions; activated; starts; ends; hexagons }

(* TODO: currently does not support BlueYellowPaths: *)
let from_puzzle context puzzle =
  let neighbors_of pos =
    Graph.adjacent_edges pos puzzle.logic_graph
    |> Edges.elements
    |> List.map (fun e -> Edge.other_end e pos)
    |> List.map (fun pos -> (pos, PosVar.find pos context.junctions))
  in
  let assertions =
    []
    ++ (* There exists a start which has value Player 0 *)
    Exists
      ( CoordTy,
        PosSet.elements context.starts,
        fun pos _ ->
          (Var (PosVar.find pos context.junctions) === Path (Player, Int 0))
            PathTy )
    ++ (* A path that is not a start cannot have an index of 0 *)
    Forall
      ( PathVarTy,
        context.junctions
        |> PosVar.filter (fun pos _ -> not (PosSet.mem pos context.starts))
        |> PosVar.bindings |> List.map snd,
        fun var _ ->
          (KindOf (Var var) =!= NoPath) KindTy
          ==> (IndexOf (Var var) =!= Int 0) IntTy )
    ++ (* There exists an end which is of kind Player *)
    Exists
      ( CoordTy,
        PosSet.elements context.ends,
        fun pos _ ->
          (KindOf (Var (PosVar.find pos context.junctions)) === Player) KindTy
      )
    ++ (* All junctions not being NoPath must have different valuations,
          i.e. there cannot be two junctions with the same kind & same index *)
    Forall
      ( PathVarTy,
        PosVar.bindings context.junctions |> List.map snd,
        fun junc other_juncs ->
          (KindOf (Var junc) =!= NoPath) KindTy
          ==> Forall
                ( PathVarTy,
                  other_juncs,
                  fun junc' _ ->
                    (KindOf (Var junc') =!= NoPath) KindTy
                    ==> (Var junc =!= Var junc') PathTy ) )
    ++ (* All junctions of kind Player or Symmetric must have a predecessor
          in their neighboring junctions, except if their index is 0 *)
    Forall
      ( CoordPathVarTy,
        PosVar.bindings context.junctions,
        fun (junc_pos, junc) _ ->
          (IndexOf (Var junc) =!= Int 0) IntTy
          ==> Exists
                ( CoordPathVarTy,
                  neighbors_of junc_pos,
                  fun (_, junc') _ ->
                    (KindOf (Var junc) === KindOf (Var junc')) KindTy
                    &&& (IndexOf (Var junc) === IndexOf (Var junc') +++ Int 1)
                          IntTy ) )
    ++ (* All symbols are activated by default. TODO: support disable symbols *)
    Forall
      ( BoolVarTy,
        PosVar.bindings context.activated |> List.map snd,
        fun var _ -> Bool var )
    ++ (* Hexagon rules. TODO: support BlueYellow property *)
    Forall
      ( CoordTy,
        PosVar.bindings context.hexagons |> List.map fst,
        fun pos _ ->
          let junction = PosVar.find pos context.junctions
          and activated = PosVar.find pos context.activated in
          Bool activated ==> (KindOf (Var junction) =!= NoPath) KindTy )
    ++?
    if PropertySet.(inter puzzle.properties symmetry_properties |> is_empty)
    then
      Some
        (Forall
           ( PathVarTy,
             PosVar.bindings context.junctions |> List.map snd,
             fun var _ -> (KindOf (Var var) =!= Symmetric) KindTy ))
    else None
  in
  assertions
(* var chemin_passe_par_arête = liste d'arêtes entre paths et arêtes entre paths et start|end : chemin *)
(* pour chaque chemin :
    |{ensemble des paires de chemins connectés}| <= 1
    i.e.
    forall x in neighbors,
         (x => \/ neighbors \ x)
      && forall y in neighbors \ x, x && y => not \/ neighbors \ {x, y} *)
(* exists start. start *)
(* exists end. end *)

(* une variable par (path | start | end) du layout
    pouvoir retrouver les coordonnées à partir d'une variable? et inversement

    deux éléments connectés -> variables nav connectées
    if not BlueYellowPath then
      type path = bool
     else
      type path = NoPath (false) | Blue | Yellow

   (* --- general rules *)

    path => deux nav connectés = path
    start => un nav connecté = meet
    end => un nav connecté = meet

    exist start. start
    exist end. end

   (* --- symbols rules *)

   (* hexagon *)

   hexagon active => hexagon color is Any => (nav under hex != NoPath)
   hexagon active => color is not Any => (nav under hex = hex color)

   (* square *)

   each cell as a variable
   a path between to cells connect the cells
   type cell-square = color != Any

   path != NoPath => connection désactivée
   color (cell connectées /\ connection active) = cell color

   square active => cell = square color

   (* star *)

   TODO

   (* shape *)

   TODO

   (* triad *)

   TODO

   (* --- properties rules *)
*)

let evaluate_quantifier (type a) (list : a list) (f : a -> a list -> 'b) =
  let rec iter acc prev = function
    | [] -> acc
    | x :: l ->
        let y = f x (prev @ l) in
        iter (y :: acc) (x :: prev) l
  in
  iter [] [] list

type types = {
  (* Basic sorts *)
  bool_sort : Z3.Sort.sort;
  int_sort : Z3.Sort.sort;
  (* Kind sort *)
  kind_sort : Z3.Sort.sort;
  make_no_path : Z3.FuncDecl.func_decl;
  make_player : Z3.FuncDecl.func_decl;
  make_symmetric : Z3.FuncDecl.func_decl;
  is_no_path : Z3.FuncDecl.func_decl;
  is_player : Z3.FuncDecl.func_decl;
  is_symmetric : Z3.FuncDecl.func_decl;
  (* Path sort *)
  path_sort : Z3.Sort.sort;
  make_path : Z3.FuncDecl.func_decl;
  get_kind : Z3.FuncDecl.func_decl;
  get_index : Z3.FuncDecl.func_decl;
}

let make_z3_types ctxt =
  (* Basic sorts *)
  let bool_sort = Z3.Boolean.mk_sort ctxt in
  let int_sort = Z3.Arithmetic.Integer.mk_sort ctxt in
  (* Kind sort *)
  let constructors =
    List.map
      (fun constr_name ->
        let recognizer = Z3.Symbol.mk_string ctxt ("is_" ^ constr_name) in
        Z3.Datatype.mk_constructor_s ctxt constr_name recognizer [] [] [])
      [ "NoPath"; "Player"; "Symmetric" ]
  in
  let kind_sort = Z3.Datatype.mk_sort_s ctxt "kind" constructors in
  let[@warning "-8"] [ make_no_path; make_player; make_symmetric ] =
    Z3.Datatype.get_constructors kind_sort
  and[@warning "-8"] [ is_no_path; is_player; is_symmetric ] =
    Z3.Datatype.get_recognizers kind_sort
  in
  (* Path tuple sort *)
  let field_symbols = Z3.Symbol.mk_strings ctxt [ "path_kind"; "path_index" ] in
  let path_sort =
    Z3.Tuple.mk_sort ctxt
      (Z3.Symbol.mk_string ctxt "path")
      field_symbols [ kind_sort; int_sort ]
  in
  let make_path = Z3.Tuple.get_mk_decl path_sort in
  let[@warning "-8"] [ get_kind; get_index ] =
    Z3.Tuple.get_field_decls path_sort
  in
  {
    bool_sort;
    int_sort;
    kind_sort;
    make_no_path;
    make_player;
    make_symmetric;
    is_no_path;
    is_player;
    is_symmetric;
    path_sort;
    make_path;
    get_kind;
    get_index;
  }

module Var = Map.Make (String)

let make_z3_vars ctxt types variables =
  let make_bool_var _ (BoolVariable var) =
    let symbol = Z3.Symbol.mk_string ctxt var in
    Var.add var (Z3.Boolean.mk_const ctxt symbol)
  in
  let make_path_var _ (PathVariable var) =
    let symbol = Z3.Symbol.mk_string ctxt var in
    Var.add var (Z3.Expr.mk_const ctxt symbol types.path_sort)
  in
  let var_makers =
    [
      PosVar.fold make_path_var variables.junctions;
      PosVar.fold make_bool_var variables.activated;
    ]
  in
  List.fold_left (fun vars maker -> maker vars) Var.empty var_makers

let assertions_to_z3 ctxt types vars =
  let rec convert_int_term = function
    | Add terms -> Z3.Arithmetic.mk_add ctxt (List.map convert_int_term terms)
    | Sub (l, r) ->
        Z3.Arithmetic.mk_sub ctxt [ convert_int_term l; convert_int_term r ]
    | Int i -> Z3.Arithmetic.Integer.mk_numeral_i ctxt i
    | IndexOf path ->
        Z3.FuncDecl.apply types.get_index [ convert_path_term path ]
  and convert_kind_term = function
    | KindOf path -> Z3.FuncDecl.apply types.get_kind [ convert_path_term path ]
    | NoPath -> Z3.FuncDecl.apply types.make_no_path []
    | Player -> Z3.FuncDecl.apply types.make_player []
    | Symmetric -> Z3.FuncDecl.apply types.make_symmetric []
  and convert_path_term = function
    | Path (kind, index) ->
        Z3.FuncDecl.apply types.make_path
          [ convert_kind_term kind; convert_int_term index ]
    | Var (PathVariable name) -> Var.find name vars
  in
  let rec convert = function
    | False -> Z3.Boolean.mk_false ctxt
    | True -> Z3.Boolean.mk_true ctxt
    | Bool (BoolVariable name) -> Var.find name vars
    | NotEqual (IntTy, l, r) ->
        Z3.Boolean.(
          mk_not ctxt (mk_eq ctxt (convert_int_term l) (convert_int_term r)))
    | NotEqual (KindTy, l, r) ->
        Z3.Boolean.(
          mk_not ctxt (mk_eq ctxt (convert_kind_term l) (convert_kind_term r)))
    | NotEqual (PathTy, l, r) ->
        Z3.Boolean.(
          mk_not ctxt (mk_eq ctxt (convert_path_term l) (convert_path_term r)))
    | Equal (IntTy, l, r) ->
        Z3.Boolean.(mk_eq ctxt (convert_int_term l) (convert_int_term r))
    | Equal (KindTy, l, r) ->
        Z3.Boolean.(mk_eq ctxt (convert_kind_term l) (convert_kind_term r))
    | Equal (PathTy, l, r) ->
        Z3.Boolean.(mk_eq ctxt (convert_path_term l) (convert_path_term r))
    | And ls -> Z3.Boolean.mk_and ctxt (List.map convert ls)
    | Or ls -> Z3.Boolean.mk_or ctxt (List.map convert ls)
    | Xor (l, r) -> Z3.Boolean.mk_xor ctxt (convert l) (convert r)
    | Imply (l, r) -> Z3.Boolean.mk_implies ctxt (convert l) (convert r)
    | Equiv (l, r) -> Z3.Boolean.mk_eq ctxt (convert l) (convert r)
    | Not l -> Z3.Boolean.mk_not ctxt (convert l)
    | Exists (_, domain, f) ->
        Z3.Boolean.mk_or ctxt (evaluate_quantifier domain f |> List.map convert)
    | Forall (_, domain, f) ->
        Z3.Boolean.mk_and ctxt (evaluate_quantifier domain f |> List.map convert)
  in
  convert

let deserialize_bool_z3_expr expr =
  match Z3.Boolean.get_bool_value expr with
  | L_FALSE -> false
  | L_TRUE -> true
  | _ -> assert false

let deserialize_path_z3_expr types expr =
  let kind_decl =
    Z3.Expr.simplify (Z3.FuncDecl.apply types.get_kind [ expr ]) None
    |> Z3.Expr.get_func_decl
  and index =
    Z3.Expr.simplify (Z3.FuncDecl.apply types.get_index [ expr ]) None
    |> Z3.Arithmetic.Integer.get_big_int |> Z.to_int
  in
  let kind =
    if Z3.FuncDecl.equal kind_decl types.make_no_path then NoPath
    else if Z3.FuncDecl.equal kind_decl types.make_player then Player
    else if Z3.FuncDecl.equal kind_decl types.make_symmetric then Symmetric
    else assert false
  in
  (kind, index)

let solve puzzle =
  let variables = make_context puzzle in
  let assertions = from_puzzle variables puzzle in
  let ctxt = Z3.mk_context [] in
  let types = make_z3_types ctxt in
  let z3_vars = make_z3_vars ctxt types variables in
  let z3_assertions =
    List.map (assertions_to_z3 ctxt types z3_vars) assertions
  in
  let solver = Z3.Solver.mk_simple_solver ctxt in
  let junc_val, act_val =
    match Z3.Solver.check solver z3_assertions with
    | Z3.Solver.SATISFIABLE -> (
        match Z3.Solver.get_model solver with
        | None -> assert false
        | Some model ->
            let junctions =
              PosVar.fold
                (fun pos (PathVariable var) ->
                  match Z3.Model.eval model (Var.find var z3_vars) false with
                  | None -> Fun.id
                  | Some expr ->
                      PosVar.add pos (deserialize_path_z3_expr types expr))
                variables.junctions PosVar.empty
            and activated =
              PosVar.fold
                (fun pos (BoolVariable var) ->
                  match Z3.Model.eval model (Var.find var z3_vars) false with
                  | None -> Fun.id
                  | Some expr -> PosVar.add pos (deserialize_bool_z3_expr expr))
                variables.activated PosVar.empty
            in
            (junctions, activated))
    | Z3.Solver.UNKNOWN -> raise (Invalid_argument "Solver: Unknown")
    | Z3.Solver.UNSATISFIABLE ->
        raise (Invalid_argument "Solver: Unsatisfiable")
  in
  let pp_kind fmt = function
    | NoPath -> Format.fprintf fmt "NoPath"
    | Player -> Format.fprintf fmt "Player"
    | Symmetric -> Format.fprintf fmt "Symmetric"
    | KindOf _ -> Format.fprintf fmt "KindOf ?Path?"
  in
  let pp_bool fmt = function
    | true -> Format.fprintf fmt "true"
    | false -> Format.fprintf fmt "false"
  in
  let pp_path fmt (kind, index) =
    Format.fprintf fmt "%a : %i" pp_kind kind index
  in
  let rec pp_map pp_val fmt map =
    match PosVar.min_binding_opt map with
    | None -> Format.fprintf fmt ""
    | Some (pos, value) ->
        Format.fprintf fmt "(%a): %a;@;%a" Coord.pp pos pp_val value
          (pp_map pp_val) (PosVar.remove pos map)
  in
  let out_file =
    open_out (Printf.sprintf "output/%s/%s.valuation" puzzle.file puzzle.name)
  in
  let fmt = Format.formatter_of_out_channel out_file in
  Format.fprintf fmt
    "Junctions:@\n@[<v 2>%a@]@\nActivated symbols:@\n@[<v 2>%a@]"
    (pp_map pp_path) junc_val (pp_map pp_bool) act_val;
  let path =
    let junc_bindings =
      PosVar.bindings junc_val |> List.map (fun (x, y) -> (y, x))
    in
    let rec iter acc i =
      let pos = List.assoc (Player, i) junc_bindings in
      if IntMap.exists (fun _ -> ( = ) pos) puzzle.ends then pos :: acc
      else iter (pos :: acc) (i + 1)
    in
    iter [] 0
  in
  path
