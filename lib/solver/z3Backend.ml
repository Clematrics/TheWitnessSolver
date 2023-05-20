open Defs
open Logic
open Problem
open Z3

[@@@warning "-69"]
type types = {
  (* Basic sorts *)
  bool_sort : Sort.sort;
  int_sort : Sort.sort;
  (* Kind sort *)
  kind_sort : Sort.sort;
  make_no_path : FuncDecl.func_decl;
  make_player : FuncDecl.func_decl;
  make_symmetric : FuncDecl.func_decl;
  is_no_path : FuncDecl.func_decl;
  is_player : FuncDecl.func_decl;
  is_symmetric : FuncDecl.func_decl;
  (* Path sort *)
  path_sort : Sort.sort;
  make_path : FuncDecl.func_decl;
  get_kind : FuncDecl.func_decl;
  get_index : FuncDecl.func_decl;
}

let make_z3_types ctxt =
  (* Basic sorts *)
  let bool_sort = Boolean.mk_sort ctxt in
  let int_sort = Arithmetic.Integer.mk_sort ctxt in
  (* Kind sort *)
  let constructors =
    List.map
      (fun constr_name ->
        let recognizer = Symbol.mk_string ctxt ("is_" ^ constr_name) in
        Datatype.mk_constructor_s ctxt constr_name recognizer [] [] [])
      [ "NoPath"; "Player"; "Symmetric" ]
  in
  let kind_sort = Datatype.mk_sort_s ctxt "kind" constructors in
  let[@warning "-8"] [ make_no_path; make_player; make_symmetric ] =
    Datatype.get_constructors kind_sort
  and[@warning "-8"] [ is_no_path; is_player; is_symmetric ] =
    Datatype.get_recognizers kind_sort
  in
  (* Path tuple sort *)
  let field_symbols = Symbol.mk_strings ctxt [ "path_kind"; "path_index" ] in
  let path_sort =
    Tuple.mk_sort ctxt
      (Symbol.mk_string ctxt "path")
      field_symbols [ kind_sort; int_sort ]
  in
  let make_path = Tuple.get_mk_decl path_sort in
  let[@warning "-8"] [ get_kind; get_index ] =
    Tuple.get_field_decls path_sort
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
    let symbol = Symbol.mk_string ctxt var in
    Var.add var (Boolean.mk_const ctxt symbol)
  in
  let make_path_var _ (PathVariable var) =
    let symbol = Symbol.mk_string ctxt var in
    Var.add var (Expr.mk_const ctxt symbol types.path_sort)
  in
  let var_makers =
    [
      PosVar.fold make_path_var variables.junctions;
      PosVar.fold make_bool_var variables.activated;
    ]
  in
  List.fold_left (fun vars maker -> maker vars) Var.empty var_makers

let assertions_to_z3 ctxt types vars =
  let rec convert_int_term : path_index expr -> Expr.expr = function
    | Add terms -> Arithmetic.mk_add ctxt (List.map convert_int_term terms)
    | Sub (l, r) ->
        Arithmetic.mk_sub ctxt [ convert_int_term l; convert_int_term r ]
    | Int i -> Arithmetic.Integer.mk_numeral_i ctxt i
    | IndexOf path -> FuncDecl.apply types.get_index [ convert_path_term path ]
  and convert_kind_term = function
    | KindOf path -> FuncDecl.apply types.get_kind [ convert_path_term path ]
    | NoPath -> FuncDecl.apply types.make_no_path []
    | Player -> FuncDecl.apply types.make_player []
    | Symmetric -> FuncDecl.apply types.make_symmetric []
  and convert_path_term = function
    | Path (kind, index) ->
        FuncDecl.apply types.make_path
          [ convert_kind_term kind; convert_int_term index ]
    | Var (PathVariable name) -> Var.find name vars
  in
  let rec convert = function
    | False -> Boolean.mk_false ctxt
    | True -> Boolean.mk_true ctxt
    | Bool (BoolVariable name) -> Var.find name vars
    | NotEqual (IntTy, l, r) ->
        Boolean.(
          mk_not ctxt (mk_eq ctxt (convert_int_term l) (convert_int_term r)))
    | NotEqual (KindTy, l, r) ->
        Boolean.(
          mk_not ctxt (mk_eq ctxt (convert_kind_term l) (convert_kind_term r)))
    | NotEqual (PathTy, l, r) ->
        Boolean.(
          mk_not ctxt (mk_eq ctxt (convert_path_term l) (convert_path_term r)))
    | Equal (IntTy, l, r) ->
        Boolean.(mk_eq ctxt (convert_int_term l) (convert_int_term r))
    | Equal (KindTy, l, r) ->
        Boolean.(mk_eq ctxt (convert_kind_term l) (convert_kind_term r))
    | Equal (PathTy, l, r) ->
        Boolean.(mk_eq ctxt (convert_path_term l) (convert_path_term r))
    | And ls -> Boolean.mk_and ctxt (List.map convert ls)
    | Or ls -> Boolean.mk_or ctxt (List.map convert ls)
    | Xor (l, r) -> Boolean.mk_xor ctxt (convert l) (convert r)
    | Imply (l, r) -> Boolean.mk_implies ctxt (convert l) (convert r)
    | Equiv (l, r) -> Boolean.mk_eq ctxt (convert l) (convert r)
    | Not l -> Boolean.mk_not ctxt (convert l)
    | Exists (_, domain, f) ->
        Boolean.mk_or ctxt (evaluate_quantifier domain f |> List.map convert)
    | Forall (_, domain, f) ->
        Boolean.mk_and ctxt (evaluate_quantifier domain f |> List.map convert)
  in
  convert

let deserialize_bool_z3_expr expr =
  match Boolean.get_bool_value expr with
  | L_FALSE -> false
  | L_TRUE -> true
  | _ -> assert false

let deserialize_path_z3_expr types expr =
  let kind_decl =
    Expr.simplify (FuncDecl.apply types.get_kind [ expr ]) None
    |> Expr.get_func_decl
  and index =
    Expr.simplify (FuncDecl.apply types.get_index [ expr ]) None
    |> Arithmetic.Integer.get_big_int |> Z.to_int
  in
  let kind =
    if FuncDecl.equal kind_decl types.make_no_path then NoPath
    else if FuncDecl.equal kind_decl types.make_player then Player
    else if FuncDecl.equal kind_decl types.make_symmetric then Symmetric
    else assert false
  in
  (kind, index)

open Z3.Solver
open Puzzle

let solve problem puzzle =
  let ctxt = mk_context [] in
  let types = make_z3_types ctxt in
  let z3_vars = make_z3_vars ctxt types problem.context in
  let z3_assertions =
    List.map (assertions_to_z3 ctxt types z3_vars) problem.assertions
  in
  let solver = mk_simple_solver ctxt in
  let junc_val, act_val =
    match check solver z3_assertions with
    | SATISFIABLE -> (
        match get_model solver with
        | None -> assert false
        | Some model ->
            let junctions =
              PosVar.fold
                (fun pos (PathVariable var) ->
                  match Model.eval model (Var.find var z3_vars) false with
                  | None -> Fun.id
                  | Some expr ->
                      PosVar.add pos (deserialize_path_z3_expr types expr))
                problem.context.junctions PosVar.empty
            and activated =
              PosVar.fold
                (fun pos (BoolVariable var) ->
                  match Model.eval model (Var.find var z3_vars) false with
                  | None -> Fun.id
                  | Some expr -> PosVar.add pos (deserialize_bool_z3_expr expr))
                problem.context.activated PosVar.empty
            in
            (junctions, activated))
    | UNKNOWN -> raise (Invalid_argument "Solver: Unknown")
    | UNSATISFIABLE -> raise (Invalid_argument "Solver: Unsatisfiable")
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
