open Puzzle
open Defs

type path = NoPath | Blue | Yellow
(* type _ atom = PathVariable : string -> path atom | Literal : path -> bool atom *)

(* type _ term = *)
(* | Equal : 'a atom * 'a atom -> bool term
   | Distinct : 'a atom * 'a atom -> bool term *)

type logical_connector =
  (* | Term of bool term *)
  | PathVariable of string
  | And of logical_connector list
  | Or of logical_connector list
  | Xor of logical_connector * logical_connector
  | Imply of logical_connector * logical_connector
  | Equiv of logical_connector * logical_connector
  | Not of logical_connector

let ( ==> ) l r = Imply (l, r)
let ( &&& ) l r = And [ l; r ]
let ( ||| ) l r = Or [ l; r ]

module PathVar = Map.Make (Edge)
module StartVar = Map.Make (Coords)
module EndVar = Map.Make (Coords)

(* TODO: currently does not support BlueYellowPaths: *)
let from_puzzle puzzle =
  let walkable_edges =
    Edges.filter
      (fun e ->
        let p, p' = Edge.get e in
        let is_usable p =
          CoordMap.find_opt p puzzle.paths |> Option.value ~default:false
        in
        is_usable p && is_usable p')
      puzzle.edges
  in
  let edges =
    Edges.fold
      (fun e ->
        let (x, y), (x', y') = Edge.get e in
        let name = Printf.sprintf "path_(%i,%i)->(%i,%i)" x y x' y' in
        PathVar.add e (PathVariable name))
      walkable_edges PathVar.empty
  in
  let starts =
    CoordMap.fold
      (fun (x, y) b ->
        if b then
          let name = Printf.sprintf "path_start(%i,%i)" x y in
          StartVar.add (x, y) (PathVariable name)
        else Fun.id)
      puzzle.starts StartVar.empty
  in
  let ends =
    IntMap.fold
      (fun _i (x, y) ->
        let name = Printf.sprintf "path_end(%i,%i)" x y in
        EndVar.add (x, y) (PathVariable name))
      puzzle.ends EndVar.empty
  in
  let assertions =
    CoordMap.fold
      (fun p b assertions ->
        if not b then assertions
        else
          let adjacent_paths =
            walkable_edges
            |> Edges.filter (Edge.is_adjacent p)
            |> Edges.to_seq
            |> Seq.map (Fun.flip PathVar.find edges)
          and start_path =
            puzzle.starts
            |> CoordMap.filter (fun p' activated -> activated && p = p')
            |> CoordMap.to_seq |> Seq.map fst
            |> Seq.map (Fun.flip StartVar.find starts)
          and end_path =
            puzzle.ends
            |> IntMap.filter (fun _ p' -> p = p')
            |> IntMap.to_seq |> Seq.map snd
            |> Seq.map (Fun.flip EndVar.find ends)
          in
          let connected_paths =
            Seq.append adjacent_paths (Seq.append start_path end_path)
          in
          (* TODO: develop operators for this, like forall, … *)
          Seq.fold_left
            (fun assertions var ->
              let connected_less_var =
                Seq.filter (( != ) var) connected_paths
              in
              Seq.fold_left
                (fun assertions var' ->
                  let connected_less_var_var' =
                    Seq.filter (( != ) var') connected_less_var
                  in
                  if Seq.is_empty connected_less_var_var' then assertions
                  else
                    (var &&& var'
                    ==> Not (Or (List.of_seq connected_less_var_var')))
                    :: assertions)
                ((var ==> Or (List.of_seq connected_less_var)) :: assertions)
                connected_less_var)
            assertions connected_paths)
      puzzle.paths []
  in
  let assertions =
    Or (ends |> StartVar.to_seq |> Seq.map snd |> List.of_seq)
    :: Or (starts |> StartVar.to_seq |> Seq.map snd |> List.of_seq)
    :: assertions
  in
  ((starts, edges, ends), assertions)
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

module Var = Map.Make (String)

let make_z3_vars ctxt =
  let rec inner vars = function
    | PathVariable s ->
        if not (Var.mem s vars) then
          let symbol = Z3.Symbol.mk_string ctxt s in
          Var.add s symbol vars
        else vars
    | And ls | Or ls -> List.fold_left inner vars ls
    | Xor (l, r) | Equiv (l, r) | Imply (l, r) ->
        List.fold_left inner vars [ l; r ]
    | Not l -> inner vars l
  in
  inner

let assertions_to_z3 ctxt vars =
  let rec convert = function
    | PathVariable s ->
        let var = Var.find s vars in
        Z3.Boolean.mk_const ctxt var
    | And ls -> Z3.Boolean.mk_and ctxt (List.map convert ls)
    | Or ls -> Z3.Boolean.mk_or ctxt (List.map convert ls)
    | Imply (l, r) -> Z3.Boolean.mk_implies ctxt (convert l) (convert r)
    | Not l -> Z3.Boolean.mk_not ctxt (convert l)
    | Equiv _ | Xor _ -> assert false
  in
  convert

let solve puzzle =
  let ctxt = Z3.mk_context [] in
  let (starts, edges, ends), assertions = puzzle |> from_puzzle in
  let vars = List.fold_left (make_z3_vars ctxt) Var.empty assertions in
  let z3_assertions = assertions |> List.map (assertions_to_z3 ctxt vars) in
  let solver = Z3.Solver.mk_simple_solver ctxt in
  (* List.iter (fun e -> Printf.printf "%s\n" (Z3.Expr.to_string e)) z3_assertions; *)
  let valuation =
    match Z3.Solver.check solver z3_assertions with
    | Z3.Solver.SATISFIABLE -> (
        match Z3.Solver.get_model solver with
        | None -> assert false
        | Some model ->
            Var.fold
              (fun name symbol valuation ->
                match
                  Z3.Model.eval model (Z3.Boolean.mk_const ctxt symbol) false
                with
                | None ->
                    (* Printf.printf "%s -> None\n" name; *)
                    valuation
                | Some expr ->
                    let e = Z3.Expr.to_string expr in
                    (* Printf.printf "%s -> %s\n" name e; *)
                    Var.add name (e = "true") valuation)
              vars Var.empty)
    | Z3.Solver.UNKNOWN -> raise (Invalid_argument "Solver: Unknown")
    | Z3.Solver.UNSATISFIABLE ->
        raise (Invalid_argument "Solver: Unsatisfiable")
  in
  let start =
    None
    |> Var.fold
         (fun s b start ->
           match start with
           | None when b ->
               starts
               |> StartVar.filter (fun _ s' -> s' = PathVariable s)
               |> StartVar.min_binding_opt |> Option.map fst
           | _ -> start)
         valuation
    |> Option.get
  in
  let end_path =
    None
    |> Var.fold
         (fun s b e ->
           match e with
           | None when b ->
               ends
               |> EndVar.filter (fun _ s' -> s' = PathVariable s)
               |> EndVar.min_binding_opt |> Option.map fst
           | _ -> e)
         valuation
    |> Option.get
  in
  let paths =
    []
    |> Var.fold
         (fun s b paths ->
           if b then
             let edge_opt =
               edges
               |> PathVar.filter (fun _ s' -> s' = PathVariable s)
               |> PathVar.min_binding_opt |> Option.map fst
             in
             match edge_opt with None -> paths | Some edge -> edge :: paths
           else paths)
         valuation
    |> List.map Edge.get
  in
  let rec iter_solution points position = function
    | [] ->
        assert (position = end_path);
        points
    | paths -> (
        let next_edge, paths =
          List.partition (fun (a, b) -> a = position || b = position) paths
        in
        match next_edge with
        | [ e ] ->
            let next = if fst e = position then snd e else fst e in
            iter_solution (next :: points) next paths
        | _ -> assert false)
  in
  let solution = iter_solution [ start ] start paths in
  solution
