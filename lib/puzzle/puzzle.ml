open Defs
open Log
module Board = Map.Make (Coords)

type path = Path of bool | Start of bool | End of int

type element = {
  path : path option;
  symbol : (Symbol.t * Color.t) option;
  connected_paths : Coords.t list;
  connected_cells : Coords.t list;
}

type t = {
  name : string;
  properties : PropertySet.t;
  width : int;
  height : int;
  board : element Board.t;
  edges : Edges.t;
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

let convert_path = function
  | Raw.Path.Meet b | Raw.Path.PathHorizontal b | Raw.Path.PathVertical b ->
      Path b
  | Raw.Path.Start b -> Start b
  | Raw.Path.End i -> End i

let make_paths paths puzzle =
  let open Coords in
  let get = make_get puzzle.width puzzle.height paths in
  let fold = make_fold puzzle.width puzzle.height paths in
  let board, edges =
    fold
      (fun pos (board, edges) -> function
        | None -> (board, edges)
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
            ( Board.add pos
                {
                  path = Some (convert_path raw_path);
                  symbol = None;
                  connected_paths;
                  connected_cells = [];
                }
                board,
              Edges.add_seq (List.to_seq new_edges) edges ))
      (puzzle.board, puzzle.edges)
  in
  { puzzle with board; edges }

(* finding cells : take all corners (path | start) and look for the following pattern
    P P P
    P / P
    P P P
   where
     P (path) stands for Path | Start
     / (corner) stands for None
   It is only necessary to check for a cell on the bottom right)
*)
let make_cells puzzle =
  let is_P = function
    | None -> false
    | Some { path; _ } ->
        Option.fold ~none:false
          ~some:(function End _ -> false | _ -> true)
          path
  and is_empty = function
    | None -> true
    | Some { path; _ } -> Option.is_none path
  in
  let paths =
    Board.filter
      (fun _ { path; _ } ->
        match path with Some (Start _ | Path _) -> true | _ -> false)
      puzzle.board
  in
  let open Coords in
  (* separate board where there are only cells *)
  let cells =
    let get coords = Board.find_opt coords puzzle.board in
    Board.fold
      (fun coords _ board ->
        let cell_pos = coords +: (1, 1) in
        let edges =
          List.map (fun offset -> get (cell_pos +: offset)) Coords.all
        in
        let cell = get cell_pos in
        if List.for_all is_P edges && is_empty cell then
          Board.add cell_pos
            {
              path = None;
              symbol = None;
              connected_paths = Coords.adjacent;
              connected_cells = [];
            }
            board
        else board)
      paths Board.empty
  in
  (* Adding connected cells to paths *)
  (* add_path_cell connection updates the path at (pos + offset) in the board
     by adding to connected_cells the cell at position pos. *)
  let add_cell_connection pos path_board offset =
    let update_path = function
      | Some elt ->
          Some
            { elt with connected_cells = (offset *: -1) :: elt.connected_cells }
      | None -> assert false
    in
    let coords = pos +: offset in
    Board.update coords update_path path_board
  in
  let board =
    puzzle.board
    |> (* for each cell, go to each adjacent path and update it *)
    Board.fold
      (fun pos cell path_board ->
        List.fold_left (add_cell_connection pos) path_board cell.connected_paths)
      cells
    |> (* Merging cells and paths *)
    Board.fold Board.add cells
  in
  { puzzle with board }

let add_symbols symbols puzzle =
  let fold = make_fold puzzle.width puzzle.height symbols in
  let board =
    fold
      (fun pos board ->
        (* Updates an element of the board, must be on an already registered element *)
        let update_element (sym, col) = function
          | Some elt -> Some { elt with symbol = Some (sym, col) }
          | None ->
              error (Msg.bad_symbol_position pos (Symbol.to_string sym));
              None
        in
        function
        | None -> board | Some sc -> Board.update pos (update_element sc) board)
      puzzle.board
  in
  { puzzle with board }

let validate ({ properties; width; height; board; _ } as puzzle) =
  let has p = PropertySet.mem p properties in
  let exists f = Board.exists (fun _ -> f) board in
  let ( => ) x y = if x then y else true in
  let paths = Board.filter_map (fun _ { path; _ } -> path) board in
  let check_symmetry end_i end_j symmetric =
    let is_symmetric = ref false in
    for j = 0 to end_j do
      for i = 0 to end_i do
        let p = Board.find_opt (i, j) paths and p', (i', j') = symmetric i j in
        match (p, p') with
        | Some (Start _), Some (Start _) | Some (End _), Some (End _) -> ()
        | Some (Start _), _ | _, Some (Start _) ->
            warn (Msg.unsymmetric "Starts" (i, j) (i', j'));
            is_symmetric := false
        | Some (End _), _ | _, Some (End _) ->
            warn (Msg.unsymmetric "Ends" (i, j) (i', j'));
            is_symmetric := false
        | _, _ -> ()
      done
    done;
    !is_symmetric
  in
  let check_vertical_symmetry is_cylindrical =
    let symmetric =
      if is_cylindrical then fun i j ->
        (Board.find_opt (i + ((width - 1) / 2), j) paths, (i, j))
      else fun i j -> (Board.find_opt (width - 1 - i, j) paths, (i, j))
    in
    check_symmetry (((width - 1) / 2) - 1) (height - 1) symmetric
  in
  let check_horizontal_symmetry _is_cylindrical =
    let symmetric i j = (Board.find_opt (i, height - 1 - j) paths, (i, j)) in
    check_symmetry (width - 1) (((height - 1) / 2) - 1) symmetric
  in
  let check_axial_symmetry is_cylindrical =
    let symmetric =
      if is_cylindrical then fun i j ->
        (Board.find_opt (i + ((width - 1) / 2), height - 1 - j) paths, (i, j))
      else fun i j ->
        (Board.find_opt (width - 1 - i, height - 1 - j) paths, (i, j))
    in
    check_symmetry (width - 1) (((height - 1) / 2) - 1) symmetric
  in
  let distinct_ends =
    let module IntSet = Set.Make (Int) in
    try
      let _ =
        Board.fold
          (fun _ { path; _ } set ->
            match path with
            | Some (End n) ->
                if IntSet.mem n set then raise (Invalid_argument "")
                else IntSet.add n set
            | _ -> set)
          board IntSet.empty
      in
      true
    with Invalid_argument _ -> false
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
    ( lazy
        (exists (function { path = Some (Start _); _ } -> true | _ -> false)),
      Msg.no_start );
    ( lazy (exists (function { path = Some (End _); _ } -> true | _ -> false)),
      Msg.no_end );
    (lazy distinct_ends, Msg.similar_ends);
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
    ( lazy
        (has Cylindrical
        => (List.init height Fun.id
           |> List.for_all (fun y ->
                  Board.(find_opt (0, y) board = find_opt (width - 1, y) board))
           )),
      Msg.cylindrical_property_unsatisfied );
  ]
  |> List.iter (fun (rule, msg) ->
         match log Lazy.force rule with
         | Ok (satisfied, h) ->
             if not satisfied then Log.propagate (h @ [ Error msg ]) else ()
         | Error _ -> assert false);
  puzzle

let from_raw ({ name; properties; width; height; paths; symbols } : Raw.t) =
  let%log puzzle =
    {
      name;
      properties;
      width;
      height;
      board = Board.empty;
      edges = Edges.empty;
    }
    |> make_paths paths |> make_cells |> add_symbols symbols |> validate
  in
  return puzzle

let from_chn chn =
  let+ raw_puzzles = Raw.from_chn chn in
  raw_puzzles
  |> List.map (fun raw -> (from_raw raw, raw))
  |> List.map (fun (log, raw) ->
         let ctxt = Printf.sprintf "In puzzle %s:" Raw.(raw.name) in
         match log with
         | Ok (res, (_ :: _ as h)) -> Ok (res, Context ctxt :: h)
         | Error (_ :: _ as h) -> Error (Context ctxt :: h)
         | _ -> log)
  |> merge
