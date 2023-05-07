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
}

let make_get width height arr (x, y) =
  if x >= 0 && x < width && y >= 0 && y < height then arr.(x).(y) else None

let make_fold width height arr f init =
  let r = ref init in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      r := f (x, y) arr.(x).(y) !r
    done
  done;
  !r

let convert_path = function
  | Raw.Path.Meet b | Raw.Path.PathHorizontal b | Raw.Path.PathVertical b ->
      Path b
  | Raw.Path.Start b -> Start b
  | Raw.Path.End i -> End i

let make_paths width height paths =
  let open Coords in
  let get = make_get width height paths in
  let fold = make_fold width height paths in
  fold (fun pos path_opt board ->
      path_opt |> function
      | None -> board
      | Some raw_path ->
          let around = Array.make_matrix 3 3 (Some raw_path) in
          List.iter
            (fun c -> around.(fst c + 1).(snd c + 1) <- get (c +: pos))
            Coords.all;
          let connected_paths = Raw.Path.get_connections pos around in
          if connected_paths = [] then
            error (Msg.no_connection pos (Raw.Path.to_string raw_path));
          Board.add pos
            {
              path = Some (convert_path raw_path);
              symbol = None;
              connected_paths;
              connected_cells = [];
            }
            board)

(* finding cells : take all corners (path | start) and look for the following pattern
    P P P
    P / P
    P P P
   where
     P (path) stands for Path | Start
     / (corner) stands for None
   It is only necessary to check for a cell on the bottom right)
*)
let make_cells board =
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
      board
  in
  let open Coords in
  (* separate board where there are only cells *)
  let cells =
    let get coords = Board.find_opt coords board in
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
  board
  |> (* for each cell, go to each adjacent path and update it *)
  Board.fold
    (fun pos cell path_board ->
      List.fold_left (add_cell_connection pos) path_board cell.connected_paths)
    cells
  |> (* Merging cells and paths *)
  Board.fold Board.add cells

let add_symbols width height symbols =
  let fold = make_fold width height symbols in
  fold (fun pos sym_opt board ->
      (* Updates an element of the board, must be on an already registered element *)
      let update_element (sym, col) = function
        | Some elt -> Some { elt with symbol = Some (sym, col) }
        | None ->
            error (Msg.bad_symbol_position pos (Symbol.to_string sym));
            None
      in
      match sym_opt with
      | None -> board
      | Some sc -> Board.update pos (update_element sc) board)

let validate x = return x

let from_raw ({ name; properties; width; height; paths; symbols } : Raw.t) =
  let%log board =
    Board.empty
    |> make_paths width height paths
    |> make_cells
    |> add_symbols width height symbols
  in
  let puzzle = { name; properties; width; height; board } in
  validate puzzle

let from_chn chn =
  let+ raw_puzzles = Raw.from_chn chn in
  raw_puzzles
  |> List.map (fun raw -> (from_raw raw, raw))
  |> List.map (fun (log, raw) ->
         let ctxt = Printf.sprintf "In puzzle %s:" Raw.(raw.name) in
         match log with
         | Ok (res, (_ :: _ as h)) -> Ok (res, ctxt :: h)
         | Error (_ :: _ as h) -> Error (ctxt :: h)
         | _ -> log)
  |> merge