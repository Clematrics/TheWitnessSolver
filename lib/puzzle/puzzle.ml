open Defs
open Log
module CoordSet = Set.Make (Coords)
module CoordMap = Map.Make (Coords)
module IntMap = Map.Make (Int)

type t = {
  file : string;
  name : string;
  properties : PropertySet.t;
  width : int;
  height : int;
  paths_ : bool CoordMap.t;
  points : CoordSet.t;
  cuts : CoordSet.t;
  edges : Edges.t;
  starts : bool CoordMap.t;
  ends : Coords.t IntMap.t;
  cells : CoordSet.t;
  symbols : (Symbol.t * Color.t) CoordMap.t;
}

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

let make_paths paths puzzle =
  let open Coords in
  let get = make_get puzzle.width puzzle.height paths in
  let fold (type a) f (i : a) =
    make_fold puzzle.width puzzle.height paths f i
  in
  let edges =
    fold
      (fun pos edges -> function
        | None -> edges
        | Some raw_path ->
            let around = Array.make_matrix 3 3 (Some raw_path) in
            List.iter
              (fun c -> around.(fst c + 1).(snd c + 1) <- get (c +: pos))
              Coords.all;
            let connected_paths = Raw.Path.get_connections pos around in
            if connected_paths = [] then
              error (Msg.no_connection pos (Raw.Path.to_string raw_path));
            let new_edges =
              List.map
                (fun offset -> Edge.edge pos (pos +: offset))
                connected_paths
            in
            Edges.add_seq (List.to_seq new_edges) edges)
      puzzle.edges
  in
  let add_end pos = function
    | None -> Some pos
    | Some _ ->
        error Msg.similar_ends;
        Some pos
  in
  let paths_, starts, ends =
    fold
      (fun pos (paths, starts, ends) -> function
        | Some (Start b) ->
            (* A disabled path can still be used indirectly by a second, symmetrically induced path *)
            (CoordMap.add pos true paths, CoordMap.add pos b starts, ends)
        | Some (End i) ->
            ( CoordMap.add pos true paths,
              starts,
              IntMap.update i (add_end pos) ends )
        | Some (Meet b) | Some (PathVertical b) | Some (PathHorizontal b) ->
            (CoordMap.add pos b paths, starts, ends)
        | None -> (paths, starts, ends))
      (puzzle.paths_, puzzle.starts, puzzle.ends)
  in
  let points, cuts =
    CoordMap.fold
      (fun pos b (points, cuts) ->
        if b then (CoordSet.add pos points, cuts)
        else (points, CoordSet.add pos cuts))
      paths_
      (CoordSet.empty, CoordSet.empty)
  in
  { puzzle with points; cuts; edges; paths_; starts; ends }

let optimize puzzle =
  let edges, to_remove =
    CoordSet.fold
      (fun pos ((edges, to_remove) as acc) ->
        let adjacents = edges |> Edges.filter (Edge.is_adjacent pos) in
        if
          Edges.cardinal adjacents = 2
          && (not (CoordMap.mem pos puzzle.starts))
          && not (CoordMap.mem pos puzzle.symbols)
        then
          let[@warning "-8"] [ e; e' ] = Edges.elements adjacents in
          if Edge.aligned e e' then
            (* Format.printf "Optimize %a and %a\n" Edge.pp e Edge.pp e'; *)
            let c, c' = (Edge.other_end e pos, Edge.other_end e' pos) in
            (* Format.printf "Replaced by %a\n" Edge.pp (Edge.edge c c'); *)
            let edges =
              edges |> Edges.remove e |> Edges.remove e'
              |> Edges.add (Edge.edge c c')
            and to_remove = to_remove |> CoordSet.add pos in
            (edges, to_remove)
          else acc
        else acc)
      puzzle.points
      (puzzle.edges, CoordSet.empty)
  in
  let points = CoordSet.diff puzzle.points to_remove in
  { puzzle with points; edges }

(* finding cells : take all corners (path | start) and look for the following pattern
    P P P
    P / P
    P P P
   where
     P (path) stands for Some Path
     / (corner) stands for None
   It is only necessary to check for a cell on the bottom right)
*)
let make_cells puzzle =
  let cells =
    let get coords = CoordMap.find_opt coords puzzle.paths_ in
    CoordMap.fold
      (fun coords _ board ->
        let open Coords in
        let cell_pos = coords +: (1, 1) in
        let cell = get cell_pos in
        let borders =
          List.map (fun offset -> get (cell_pos +: offset)) Coords.all
        in
        if List.for_all Option.is_some borders && Option.is_none cell then
          CoordSet.add cell_pos board
        else board)
      puzzle.paths_ CoordSet.empty
  in
  { puzzle with cells }

let add_symbols symbols puzzle =
  let fold = make_fold puzzle.width puzzle.height symbols in
  let symbols =
    fold
      (fun pos symbols -> function
        | None -> symbols
        | Some sc -> CoordMap.add pos sc symbols)
      puzzle.symbols
  in
  { puzzle with symbols }

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

let from_raw file ({ name; properties; width; height; paths; symbols } : Raw.t)
    =
  let%log puzzle =
    {
      file;
      name;
      properties;
      width;
      height;
      paths_ = CoordMap.empty;
      points = CoordSet.empty;
      cuts = CoordSet.empty;
      edges = Edges.empty;
      starts = CoordMap.empty;
      ends = IntMap.empty;
      cells = CoordSet.empty;
      symbols = CoordMap.empty;
    }
    |> make_paths paths |> make_cells |> add_symbols symbols |> optimize
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
