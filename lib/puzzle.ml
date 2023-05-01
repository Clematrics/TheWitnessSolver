open Definition
open RawPuzzle
module Board = Map.Make (Coords)

type element = {
  path : nav option;
  symbol : (symbol * color) option;
  connected_paths : OffsetSet.t;
  connected_cells : OffsetSet.t;
}

type t = {
  name : string;
  properties : PropertySet.t;
  width : int;
  height : int;
  board : element Board.t;
}

(* Temp module and type to deal with assignments during the conversion
   from raw puzzles to layouts and symbols *)
module AssignmentMap = Map.Make (Char)

type assignments = (nav option * (symbol * color) option) AssignmentMap.t

(* Default assignments:
   - the space character is replaced by nothing
*)
let init_assignments : assignments =
  AssignmentMap.add ' ' (None, None) AssignmentMap.empty

open Log

(* Split rules into their respective categories
   *)
let merge_rules (init : PropertySet.t * assignments) rules :
    PropertySet.t * assignments =
  (* TODO: Log when overiding a rule ? *)
  let properties, assignments =
    List.fold_left
      (fun (set, ass) -> function
        | Rule.Property x -> (PropertySet.add x set, ass)
        | Rule.Assignment (c, nav, sym) ->
            (set, AssignmentMap.add c (nav, sym) ass))
      init rules
  in
  (properties, assignments)

let path_connection = function
  | Meet -> OffsetSet.adjacent
  | Start _ -> OffsetSet.adjacent
  | End _ -> OffsetSet.all
  | PathHorizontal _ -> OffsetSet.horizontal
  | PathVertical _ -> OffsetSet.vertical

let optional_connection = function
  | Start _ | Meet -> true
  | End _ -> true (* TODO: special case *)
  | PathHorizontal _ | PathVertical _ -> false

let string_of_path = function
  | Meet -> "Meet"
  | Start _ -> "Start"
  | End i -> Printf.sprintf "End %i" i
  | PathHorizontal false -> "PathHorizontal:Cut"
  | PathHorizontal true -> "PathHorizontal"
  | PathVertical false -> "PathVertical:Cut"
  | PathVertical true -> "PathVertical"

let string_of_offset =
  let open Offset in
  function
  | SxPy -> "above"
  | SxNy -> "below"
  | PxPy -> "to its bottom right corner"
  | NxNy -> "to its top left corner"
  | PxSy -> "to its left"
  | NxSy -> "to its right"
  | NxPy -> "to its top right corner"
  | PxNy -> "to its bottom left corner"

let string_of_symbol = function
  | Hexagon -> "Hexagon"
  | Square -> "Square"
  | Star -> "Star"
  | Shape _ -> "Shape"
  | AntiShape _ -> "AntiShape"
  | Triad -> "Triad"
  | Triangle _ -> "Triangle"

module Error = struct
  let coords () (x, y) = Printf.sprintf "At line %i column %i:" (y + 1) (x + 1)

  let unknown_assignment =
    Printf.sprintf "%a Cannot find an assignment for character %c" coords

  let missing_connection xy path offset =
    Printf.sprintf "%a Path '%s' must be connected to the path %s" coords xy
      (string_of_path path) (string_of_offset offset)

  let no_connection xy path =
    Printf.sprintf "%a Path '%s' is not connected to anything" coords xy
      (string_of_path path)

  let ambiguous_end_connection =
    Printf.sprintf
      "%a Ambiguous connections for the End %i. End can only be connected to \
       one other path"
      coords

  let too_much_end_connections =
    Printf.sprintf "%a The End %i is not connected to any other path" coords

  let bad_symbol_position xy sym =
    Printf.sprintf "%a The symbol '%s' is neither in a cell nor on a path"
      coords xy (string_of_symbol sym)
end

let matrix_view (width, height) assignments lines =
  let layout = Array.make_matrix width height None in
  let symbols = Array.make_matrix width height None in
  lines
  |> List.iteri (fun y line ->
         line
         |> String.iteri (fun x c ->
                let nav, sym =
                  try AssignmentMap.find c assignments
                  with Not_found ->
                    error (Error.unknown_assignment (x, y) c);
                    (None, None)
                in
                layout.(x).(y) <- nav;
                symbols.(x).(y) <- sym));
  let get arr (x, y) =
    if x >= 0 && x < width && y >= 0 && y < height then arr.(x).(y) else None
  in
  let fold_xy arr f init =
    let _, v =
      Array.fold_left
        (fun (y, v) line ->
          let _, v' =
            Array.fold_left
              (fun (x, v) elt ->
                let v' = f (x, y) elt v in
                (x + 1, v'))
              (0, v) line
          in
          (y + 1, v'))
        (0, init) arr
    in
    v
  in
  (get layout, fold_xy layout, get symbols, fold_xy symbols)

let inject_paths get_layout fold_layout =
  let open Coords in
  fold_layout (fun (x, y) nav_opt board ->
      nav_opt
      |> Option.fold ~none:board ~some:(fun path ->
             let offsets = path_connection path in
             let opt = optional_connection path in
             let connected_paths =
               OffsetSet.filter_map
                 (fun o ->
                   let coords = (x, y) +: Offset.map_coords o in
                   let ( let* ) = Fun.flip Option.map in
                   let* p = get_layout coords in
                   let connected =
                     OffsetSet.mem (Offset.opposite o) (path_connection p)
                   in
                   if (not opt) && not connected then
                     error (Error.missing_connection (x, y) path o);
                   o)
                 offsets
             in
             (* check connected to something *)
             if OffsetSet.cardinal connected_paths = 0 then
               error (Error.no_connection (x, y) path);
             (* special handling of End paths *)
             let connected_paths =
               match path with
               | End i -> (
                   let adjacent =
                     OffsetSet.filter
                       (Fun.flip OffsetSet.mem OffsetSet.adjacent)
                       connected_paths
                   in
                   match OffsetSet.cardinal adjacent with
                   | 0 ->
                       (* Look for a corner *)
                       let corners =
                         OffsetSet.filter
                           (Fun.flip OffsetSet.mem OffsetSet.corners)
                           connected_paths
                       in
                       (match OffsetSet.cardinal corners with
                       | 1 -> (* ok *) ()
                       | 0 ->
                           (* redondant with the connection check above *)
                           error (Error.too_much_end_connections (x, y) i)
                       | _ -> error (Error.ambiguous_end_connection (x, y) i));
                       corners
                   | 1 -> (* ok *) adjacent
                   | _ ->
                       error (Error.ambiguous_end_connection (x, y) i);
                       adjacent)
               | _ -> connected_paths
             in
             Board.add (x, y)
               {
                 path = Some path;
                 symbol = None;
                 connected_paths;
                 connected_cells = OffsetSet.empty;
               }
               board))
(* for y = 0 to height - 1 do
     for x = 0 to width - 1 do
       get_layout (x, y)
       |>
     done
   done;
   !board *)

let make_cells board =
  (* finding cells : find all corners (meet | start) and look for the following pattern
      C H C
      V / V
      C H C
     where
       C (corner) stands for Meet | Start
       H (horizontal) stands for Meet | Start | PathHorizontal | CutPathHorizontal
       V (vertical) stands for Meet | Start | PathVertical | CutPathVertical
       / (corner) stands for None
     It is only necessary to check for a cell on the bottom right)
  *)
  let is_C { path; _ } =
    Option.fold ~none:false
      ~some:(function Start _ | Meet -> true | _ -> false)
      path
  and is_V { path; _ } =
    Option.fold ~none:false
      ~some:(function Start _ | Meet | PathVertical _ -> true | _ -> false)
      path
  and is_H { path; _ } =
    Option.fold ~none:false
      ~some:(function Start _ | Meet | PathHorizontal _ -> true | _ -> false)
      path
  and is_empty = function
    | None -> true
    | Some { path; _ } -> Option.is_none path
  in
  let corners =
    Board.filter
      (fun _ { path; _ } ->
        match path with Some Meet | Some (Start _) -> true | _ -> false)
      board
  in
  let open Coords in
  let cells =
    let get coords = Board.find_opt coords board in
    Board.filter_map
      (fun coords _ ->
        let ( let* ) = Option.bind in
        let* top = get (coords +: (1, 0)) in
        let* bottom = get (coords +: (1, 2)) in
        let* left = get (coords +: (0, 1)) in
        let* right = get (coords +: (2, 1)) in
        let* c1 = get (coords +: (2, 0)) in
        let* c2 = get (coords +: (2, 2)) in
        let* c3 = get (coords +: (0, 2)) in
        let cell = get (coords +: (1, 1)) in
        if
          List.for_all Fun.id
            [
              is_H top;
              is_H bottom;
              is_V left;
              is_V right;
              is_C c1;
              is_C c2;
              is_C c3;
              is_empty cell;
            ]
        then
          Some
            {
              path = None;
              symbol = None;
              (* TODO: change symbol *) connected_paths = OffsetSet.adjacent;
              connected_cells = OffsetSet.empty;
            }
        else None)
      corners
  in
  (* Adding connected cells to paths *)
  let board =
    Board.fold
      (fun k v b ->
        OffsetSet.fold
          (fun o b ->
            let coords = k +: (1, 1) +: Offset.map_coords o in
            Board.update coords
              (function
                | Some elt ->
                    Some
                      {
                        elt with
                        connected_cells =
                          OffsetSet.add (Offset.opposite o) elt.connected_cells;
                      }
                | None -> assert false)
              b)
          v.connected_paths b)
      cells board
  in
  (* Adding cells *)
  let board =
    Board.fold (fun k v b -> Board.add (k +: (1, 1)) v b) cells board
  in
  board

let add_symbols fold_symbol =
  (* Adding symbols *)
  fold_symbol (fun (x, y) sym_opt board ->
      sym_opt
      |> Option.fold ~none:board ~some:(fun (s, c) ->
             Board.update (x, y)
               (function
                 | Some elt -> Some { elt with symbol = Some (s, c) }
                 | None ->
                     error (Error.bad_symbol_position (x, y) s);
                     None)
               board))

let board_from_raw assignments raw =
  let ((width, height) as dim) = RawPuzzle.get_dimension raw in
  let get_layout, fold_layout, _, fold_symbol =
    matrix_view dim assignments raw.lines
  in
  let board =
    Board.empty
    |> inject_paths get_layout fold_layout
    |> make_cells |> add_symbols fold_symbol
  in
  (width, height, board)

let validate x = return x

let from_raw global_rules raw =
  let%log properties, assignments =
    raw.rules |> List.map Rule.from_raw |> merge_rules global_rules
  in
  let%log width, height, board = board_from_raw assignments raw in
  let puzzle = { name = raw.name; properties; width; height; board } in
  validate puzzle

let from_chn chn =
  let raw_global_rules, raw_puzzles = RawPuzzle.from_chn chn in
  let%log global_rules =
    raw_global_rules |> List.map Rule.from_raw
    |> merge_rules (PropertySet.empty, init_assignments)
  in
  raw_puzzles |> List.map (from_raw global_rules) |> merge
