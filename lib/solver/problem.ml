open Defs
module EdgesVar = Map.Make (Edge)
module PosSet = CoordSet
module PosVar = CoordMap
open Logic

type _ quty += CoordTy : Coord.t quty
type _ quty += BoolVarTy : bool var quty
type _ quty += PathVarTy : path var quty
type _ quty += CoordPathVarTy : (Coord.t * path var) quty
type _ quty += CoordZoneVarTy : (Coord.t * zone var) quty
type _ quty += EdgeBoolVarTy : (Edge.t * bool var) quty

type context = {
  junctions : path var PosVar.t;
  edges : bool var EdgesVar.t;
  (* Starts (usable by the player) & Ends positions *)
  starts : PosSet.t;
  ends : PosSet.t;
  (* cells *)
  cells : cell var PosVar.t;
  cells_id : int PosVar.t;
  cells_zone : zone var PosVar.t;
  (* symbols *)
  activated : bool var PosVar.t;  (** Whether symbols are activated or not *)
  hexagons : Color.t PosVar.t;
}

open Puzzle

let make_context puzzle =
  let junctions =
    Graph.fold_points
      (fun (x, y) ->
        let name = Printf.sprintf "path_(%i,%i)" x y in
        PosVar.add (x, y) (PathVariable name))
      puzzle.logic_graph PosVar.empty
  and edges =
    Graph.fold_edges
      (fun edge ->
        let (x, y), (x', y') = Edge.get edge in
        let name = Printf.sprintf "edge_(%i,%i)->(%i,%i)" x y x' y' in
        EdgesVar.add edge (BoolVariable name))
      puzzle.logic_graph EdgesVar.empty
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
  and cells =
    PosSet.fold
      (fun (x, y) ->
        let name = Printf.sprintf "cell(%i,%i)" x y in
        PosVar.add (x, y) (CellVariable name))
      puzzle.cells PosVar.empty
  and _, cells_id =
    PosSet.fold
      (fun (x, y) (next_id, vars) ->
        (next_id + 1, PosVar.add (x, y) next_id vars))
      puzzle.cells (0, PosVar.empty)
  and cells_zone =
    PosSet.fold
      (fun (x, y) ->
        let name = Printf.sprintf "cell_zone(%i,%i)" x y in
        PosVar.add (x, y) (ZoneVariable name))
      puzzle.cells PosVar.empty
  in
  {
    junctions;
    edges;
    starts;
    ends;
    cells;
    cells_id;
    cells_zone;
    activated;
    hexagons;
  }

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
    ++ (* All junctions of kind NoPath have index 0 *)
    Forall
      ( PathVarTy,
        context.junctions |> PosVar.bindings |> List.map snd,
        fun var _ ->
          (KindOf (Var var) === NoPath) KindTy
          ==> (IndexOf (Var var) === Int 0) IntTy )
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
    ++ (* A path goes through an edge if and only if
          the ends of this edge are of the same kind
          and with indexes one apart *)
    Forall
      ( EdgeBoolVarTy,
        EdgesVar.bindings context.edges,
        fun (edge, edge_var) _ ->
          let p, p' = Edge.get edge in
          let p_var, p'_var =
            PosVar.(find p context.junctions, find p' context.junctions)
          in
          (* No need to check if they are NoPath, since indexes of NoPath are always 0 *)
          (KindOf (Var p_var) === KindOf (Var p'_var)) KindTy
          &&& ((IndexOf (Var p_var) +++ Int 1 === IndexOf (Var p'_var)) IntTy
              ||| (IndexOf (Var p_var) === IndexOf (Var p'_var) +++ Int 1) IntTy
              )
          <=> Bool edge_var )
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
    ++? (if
           PropertySet.(inter puzzle.properties symmetry_properties |> is_empty)
         then
           Some
             (Forall
                ( PathVarTy,
                  PosVar.bindings context.junctions |> List.map snd,
                  fun var _ -> (KindOf (Var var) =!= Symmetric) KindTy ))
         else None)
    ++ (* Forall cells, the cell zone is the minimum of the cell zone of its neighbor and its cell starting id *)
    Forall
      ( CoordTy,
        PosVar.bindings context.cells |> List.map fst,
        fun pos other_cells ->
          Forall
            ( CoordTy,
              other_cells,
              fun pos' _ ->
                let open Coord in
                let offsets = [ (0, 1); (1, 0); (-1, 0); (0, -1) ] in
                let neighbors =
                  List.map (fun o -> (pos +: (o *: 2), o)) offsets
                in
                match List.assoc_opt pos' neighbors with
                | None ->
                    (* Not a neighbor *)
                    Not (Neighbor (pos, pos'))
                | Some edge_offset -> (
                    match
                      Graph.edge_through (pos +: edge_offset) puzzle.logic_graph
                    with
                    | None -> Neighbor (pos, pos')
                    | Some edge ->
                        let edge_var = EdgesVar.find edge context.edges in
                        Not (Bool edge_var) <=> Neighbor (pos, pos')) ) )
    ++ (* Two cells connected is equivalent to be in the same zone.
          This must be a global property, against all pairs of cells.
          Otherwise we can get a situation where there can be 3 zones
          but only two identifiers {1,2} with the separation 1|2|1 *)
    Forall
      ( CoordZoneVarTy,
        PosVar.bindings context.cells_zone,
        fun (pos, cell) other_cells ->
          Forall
            ( CoordZoneVarTy,
              other_cells,
              fun (pos', cell') _ ->
                (Zone cell === Zone cell') IntTy <=> Connected (pos, pos') ) )
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

type t = { context : context; assertions : bool expr list }

let logic_problem_of puzzle =
  let context = make_context puzzle in
  let assertions = from_puzzle context puzzle in
  { context; assertions }
