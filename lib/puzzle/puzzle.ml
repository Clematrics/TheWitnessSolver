open Defs
open Log
module IntMap = Map.Make (Int)

type t = {
  file : string;
  name : string;
  properties : PropertySet.t;
  width : int;
  height : int;
  logic_graph : Graph.t;
  graph : Graph.t;
  cuts_graph : Graph.t;
  starts : bool CoordMap.t;
  ends : Coord.t IntMap.t;
  cells : CoordSet.t;
  symbols : (Symbol.t * Color.t) CoordMap.t;
}

type puzzle_base = {
  file : string;
  name : string;
  properties : PropertySet.t;
  width : int;
  height : int;
}

type puzzle_graphs = {
  graph : Graph.t;
  cuts_graph : Graph.t;
  starts : bool CoordMap.t;
  ends : Coord.t IntMap.t;
}

type puzzle_cells = { cells : CoordSet.t }
type puzzle_symbs = { symbols : (Symbol.t * Color.t) CoordMap.t }
type puzzle_optimized = { logic_graph : Graph.t }

type _ under_construction =
  | Stage1 : puzzle_base -> [ `Stage1 ] under_construction
  | Stage2 : puzzle_base * puzzle_graphs -> [ `Stage2 ] under_construction
  | Stage3 :
      puzzle_base * puzzle_graphs * puzzle_cells
      -> [ `Stage3 ] under_construction
  | Stage4 :
      puzzle_base * puzzle_graphs * puzzle_cells * puzzle_symbs
      -> [ `Stage4 ] under_construction
  | Stage5 :
      puzzle_base
      * puzzle_graphs
      * puzzle_cells
      * puzzle_symbs
      * puzzle_optimized
      -> [ `Stage5 ] under_construction

let make_get width height arr (x, y) =
  if x >= 0 && x < width && y >= 0 && y < height then arr.(x).(y) else None

let make_fold width height arr f init =
  let r = ref init in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      r := f (x, y) !r arr.(x).(y)
    done
  done;
  !r

let make_paths paths (Stage1 ({ width; height; _ } as base)) =
  let open Coord in
  let get = make_get width height paths in
  let fold (type a) f (i : a) = make_fold width height paths f i in

  let make_edges pos raw_path =
    let around = Array.make_matrix 3 3 (Some raw_path) in
    List.iter
      (fun c -> around.(fst c + 1).(snd c + 1) <- get (c +: pos))
      Coord.all;
    let connected_paths = Raw.Path.get_connections pos around in
    if connected_paths = [] then
      error (Msg.no_connection pos (Raw.Path.to_string raw_path));
    let new_edges =
      List.map (fun offset -> Edge.edge pos (pos +: offset)) connected_paths
    in
    new_edges
  in
  let convert_raw_path pos graphs raw_path =
    let new_edges = make_edges pos raw_path in
    let add = Graph.add_edges new_edges in
    let graph, cuts_graph =
      match raw_path with
      | Start _ | End _ | Meet true | PathHorizontal true | PathVertical true ->
          (add graphs.graph, graphs.cuts_graph)
      | Meet false | PathHorizontal false | PathVertical false ->
          (graphs.graph, add graphs.cuts_graph)
    in
    let starts =
      match raw_path with
      | Start b -> CoordMap.add pos b graphs.starts
      | _ -> graphs.starts
    in
    let ends =
      match raw_path with
      | End i ->
          IntMap.update i
            (function
              | None -> Some pos
              | Some _ ->
                  error Msg.similar_ends;
                  Some pos)
            graphs.ends
      | _ -> graphs.ends
    in
    { graph; cuts_graph; starts; ends }
  in
  let graphs =
    fold
      (fun pos puzzle ->
        Option.fold ~none:puzzle ~some:(convert_raw_path pos puzzle))
      {
        graph = Graph.empty;
        cuts_graph = Graph.empty;
        starts = CoordMap.empty;
        ends = IntMap.empty;
      }
  in
  let graph =
    Graph.fold_edges Graph.remove_edge graphs.cuts_graph graphs.graph
  in
  Stage2 (base, { graphs with graph })

let optimize (Stage4 (base, graphs, cells, symbols)) =
  let rec remove_deadends graph =
    let deadends =
      Graph.arity_map graph
      |> CoordMap.filter (fun _ -> ( = ) 1)
      |> CoordMap.filter (fun p _ ->
             not
               (CoordMap.mem p graphs.starts
               || IntMap.exists (fun _ -> ( = ) p) graphs.ends))
    in
    if CoordMap.is_empty deadends then graph
    else
      graph
      |> CoordMap.fold (fun p _ -> Graph.remove p) deadends
      |> remove_deadends
  in
  let logic_graph =
    graphs.graph
    |> (* remove dead-ends *)
    remove_deadends
    |> (* fuse aligned edges adjacent to points with arity 2 *)
    Graph.fold_points
      (fun pos graph ->
        let adjacents = Graph.adjacent_edges pos graph in
        if
          Edges.cardinal adjacents = 2
          && (not (CoordMap.mem pos graphs.starts))
          && not (CoordMap.mem pos symbols.symbols)
        then
          let[@warning "-8"] [ e; e' ] = Edges.elements adjacents in
          if Edge.aligned e e' then
            (* Format.printf "Optimize %a and %a\n" Edge.pp e Edge.pp e'; *)
            let c, c' = (Edge.other_end e pos, Edge.other_end e' pos) in
            (* Format.printf "Replaced by %a\n" Edge.pp (Edge.edge c c'); *)
            graph |> Graph.remove pos |> Graph.add_edge (Edge.edge c c')
          else graph
        else graph)
      graphs.graph
  in
  Stage5 (base, graphs, cells, symbols, { logic_graph })

(* finding cells : take all corners (path | start) and look for the following pattern
    P P P
    P / P
    P P P
   where
     P (path) stands for Some Path
     / (corner) stands for None
   It is only necessary to check for a cell on the bottom right)
*)
let make_cells (Stage2 (base, graphs)) =
  let cells =
    let mem p = Graph.mem graphs.graph p || Graph.mem graphs.cuts_graph p in
    CoordSet.fold
      (fun coords cells ->
        let open Coord in
        let cell_pos = coords +: (1, 1) in
        if
          List.for_all (fun offset -> mem (cell_pos +: offset)) Coord.all
          && not (mem cell_pos)
        then CoordSet.add cell_pos cells
        else cells)
      (Graph.points graphs.graph)
      CoordSet.empty
  in
  Stage3 (base, graphs, { cells })

let add_symbols symbols (Stage3 (base, graphs, cells)) =
  let fold = make_fold base.width base.height symbols in
  let symbols =
    fold
      (fun pos symbols -> function
        | None -> symbols
        | Some sc -> CoordMap.add pos sc symbols)
      CoordMap.empty
  in
  Stage4 (base, graphs, cells, { symbols })

(* TODO: check symbols are at a good position *)
let validate ({ properties; width; height; ends; starts; _ } as puzzle) =
  let has p = PropertySet.mem p properties in
  let ( => ) x y = if x then y else true in
  let check_symmetry end_i end_j symmetric =
    let is_symmetric = ref false in
    for j = 0 to end_j do
      for i = 0 to end_i do
        let pos = (i, j) and pos' = symmetric i j in
        (* Check start symmetry *)
        (match
           (CoordMap.find_opt pos starts, CoordMap.find_opt pos' starts)
         with
        | Some _, Some _ | None, None -> ()
        | _ ->
            warn (Msg.unsymmetric "Starts" pos pos');
            is_symmetric := false);
        (* Check end symmetry *)
        if
          IntMap.exists (fun _ -> ( = ) pos) ends
          != IntMap.exists (fun _ -> ( = ) pos') ends
        then (
          warn (Msg.unsymmetric "Ends" pos pos');
          is_symmetric := false)
      done
    done;
    !is_symmetric
  in
  let check_vertical_symmetry is_cylindrical =
    let symmetric =
      if is_cylindrical then fun i j -> (i + ((width - 1) / 2), j)
      else fun i j -> (width - 1 - i, j)
    in
    check_symmetry (((width - 1) / 2) - 1) (height - 1) symmetric
  in
  let check_horizontal_symmetry _is_cylindrical =
    let symmetric i j = (i, height - 1 - j) in
    check_symmetry (width - 1) (((height - 1) / 2) - 1) symmetric
  in
  let check_axial_symmetry is_cylindrical =
    let symmetric =
      if is_cylindrical then fun i j -> (i + ((width - 1) / 2), height - 1 - j)
      else fun i j -> (width - 1 - i, height - 1 - j)
    in
    check_symmetry (width - 1) (((height - 1) / 2) - 1) symmetric
  in
  (* check rules
     Exists Start
     Exists End
     End:0 X ... X End:n
     VerticalSymmetry => Forall Start on left side, Start' symmetric && Forall End on left side, End' symmetric
     HorizontalSymmetry => Forall Start on up side, Start' symmetric && Forall End on up side, End' symmetric
     AxialSymmetry => Forall Start on one side, Start' symmetric && Forall End on one side, End' symmetric
     AxialSymmetry X (VerticalSymmetry || HorizontalSymmetry)
     BlueYellowPath => VerticalSymmetry || HorizontalSymmetry || AxialSymmetry
     Cylindrical => First column = Last column
  *)
  [
    (lazy (not (CoordMap.is_empty starts)), Msg.no_start);
    (lazy (not (IntMap.is_empty ends)), Msg.no_end);
    ( lazy (has VerticalSymmetry => check_vertical_symmetry (has Cylindrical)),
      Msg.vertical_symmetry_unsatisfied );
    ( lazy
        (has HorizontalSymmetry => check_horizontal_symmetry (has Cylindrical)),
      Msg.horizontal_symmetry_unsatisfied );
    ( lazy (has AxialSymmetry => check_axial_symmetry (has Cylindrical)),
      Msg.axial_symmetry_unsatisfied );
    ( lazy
        (has AxialSymmetry
        => not (has VerticalSymmetry || has HorizontalSymmetry)),
      Msg.incompatible_symmetry_properties "AxialSymmetry"
        "VerticalSymmetry or HorizontalSymmetry" );
    ( lazy
        ((has VerticalSymmetry || has HorizontalSymmetry)
        => not (has AxialSymmetry)),
      Msg.incompatible_symmetry_properties
        "VerticalSymmetry or HorizontalSymmetry" "AxialSymmetry" );
    ( lazy
        (has BlueYellowPath
        => List.exists has
             [ VerticalSymmetry; HorizontalSymmetry; AxialSymmetry ]),
      Msg.missing_symmetry );
  ]
  |> List.iter (fun (rule, msg) ->
         match log Lazy.force rule with
         | Ok (satisfied, h) ->
             if not satisfied then Log.propagate (h @ [ Error msg ]) else ()
         | Error _ -> assert false);
  puzzle

let pack
    (Stage5
      ( { file; name; properties; width; height },
        { graph; cuts_graph; starts; ends },
        { cells },
        { symbols },
        { logic_graph } )) =
  {
    file;
    name;
    properties;
    width;
    height;
    logic_graph;
    graph;
    cuts_graph;
    starts;
    ends;
    cells;
    symbols;
  }

let from_raw file ({ name; properties; width; height; paths; symbols } : Raw.t)
    =
  let%log puzzle =
    Stage1 { file; name; properties; width; height }
    |> make_paths paths |> make_cells |> add_symbols symbols |> optimize |> pack
    |> validate
  in
  return puzzle

let from_chn ?(filename = "") chn =
  let+ raw_puzzles = Raw.from_chn chn in
  raw_puzzles
  |> List.map (fun raw -> (from_raw filename raw, raw))
  |> List.map (fun (log, raw) ->
         let ctxt = Printf.sprintf "In puzzle %s:" Raw.(raw.name) in
         match log with
         | Ok (res, (_ :: _ as h)) -> Ok (res, Context ctxt :: h)
         | Error (_ :: _ as h) -> Error (Context ctxt :: h)
         | _ -> log)
  |> merge
