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
let split_rules init_properties (init_assignments : assignments) rules :
    (PropertySet.t * assignments) log_result =
  log
    (fun () ->
      let properties, assignments =
        List.fold_left
          (fun (set, ass) -> function
            | Rule.Property x -> (PropertySet.add x set, ass)
            | Rule.Assignment (c, nav, sym) ->
                (set, AssignmentMap.add c (nav, sym) ass))
          (init_properties, init_assignments)
          rules
      in
      (properties, assignments))
    ()

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

let from_raw_unchecked global_properties global_assignments raw =
  let+ properties, assignments =
    split_rules global_properties global_assignments
      (List.map Rule.from_raw raw.rules)
  in
  log
    (fun () ->
      let width, height = RawPuzzle.get_dimension raw in
      let layout = Array.make_matrix width height None in
      let symbols = Array.make_matrix width height None in
      let lines = Array.of_list raw.lines in
      for y = 0 to height - 1 do
        lines.(y)
        |> String.iteri (fun x c ->
               let nav, sym =
                 try AssignmentMap.find c assignments
                 with Not_found ->
                   error
                     (Printf.sprintf
                        "Cannot find character %c at line %i column %i" c
                        (y + 1) (x + 1));
                   (None, None)
               in
               layout.(x).(y) <- nav;
               symbols.(x).(y) <- sym)
      done;
      let get arr (x, y) =
        if x >= 0 && x < width && y >= 0 && y < height then arr.(x).(y)
        else None
      in
      let board = ref Board.empty in
      let open Coords in
      for y = 0 to height - 1 do
        for x = 0 to width - 1 do
          get layout (x, y)
          |> Option.iter (fun path ->
                 let offsets = path_connection path in
                 let opt = optional_connection path in
                 let connected_paths =
                   OffsetSet.filter_map
                     (fun o ->
                       let coords = (x, y) +: Offset.map_coords o in
                       let ( let* ) = Fun.flip Option.map in
                       let* p = get layout coords in
                       let connected =
                         OffsetSet.mem (Offset.opposite o) (path_connection p)
                       in
                       if (not opt) && not connected then
                         error
                           (Printf.sprintf
                              "Path '%s' at coord (%i, %i) must be connected \
                               to the path %s"
                              (string_of_path path) x y (string_of_offset o));
                       o)
                     offsets
                 in
                 (* check connected to something *)
                 if OffsetSet.cardinal connected_paths = 0 then
                   error
                     (Printf.sprintf
                        "Path '%s' at coord (%i, %i) is not connected to \
                         anything"
                        (string_of_path path) x y);
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
                               error
                                 (Printf.sprintf
                                    "The End %i is not connected to any other \
                                     path"
                                    i)
                           | _ ->
                               error
                                 (Printf.sprintf
                                    "Ambiguous connections for the End %i. End \
                                     can only be connected to one other path"
                                    i));
                           corners
                       | 1 -> (* ok *) adjacent
                       | _ ->
                           error
                             (Printf.sprintf
                                "Ambiguous connections for the End %i. End can \
                                 only be connected to one other path"
                                i);
                           adjacent)
                   | _ -> connected_paths
                 in
                 board :=
                   Board.add (x, y)
                     {
                       path = Some path;
                       symbol = None;
                       connected_paths;
                       connected_cells = OffsetSet.empty;
                     }
                     !board;
                 ())
        done
      done;
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
          ~some:(function
            | Start _ | Meet | PathVertical _ -> true | _ -> false)
          path
      and is_H { path; _ } =
        Option.fold ~none:false
          ~some:(function
            | Start _ | Meet | PathHorizontal _ -> true | _ -> false)
          path
      and is_empty = function
        | None -> true
        | Some { path; _ } -> Option.is_none path
      in
      let corners =
        Board.filter
          (fun _ { path; _ } ->
            match path with Some Meet | Some (Start _) -> true | _ -> false)
          !board
      in
      let cells =
        let get coords = Board.find_opt coords !board in
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
                              OffsetSet.add (Offset.opposite o)
                                elt.connected_cells;
                          }
                    | None -> assert false)
                  b)
              v.connected_paths b)
          cells !board
      in
      (* Adding cells *)
      let board =
        Board.fold (fun k v b -> Board.add (k +: (1, 1)) v b) cells board
      in
      (* Adding symbols *)
      let board = ref board in
      for y = 0 to height - 1 do
        for x = 0 to width - 1 do
          get symbols (x, y)
          |> Option.iter (fun (s, c) ->
                 board :=
                   Board.update (x, y)
                     (function
                       | Some elt -> Some { elt with symbol = Some (s, c) }
                       | None ->
                           error
                             (Printf.sprintf
                                "The symbol at coords (%i, %i) is neither in a \
                                 cell nor on a path"
                                (* TODO: add symbol description *) x y);
                           None)
                     !board)
        done
      done;
      let board = !board in
      {
        name = raw.name;
        properties;
        width;
        height;
        board (* layout; symbols *);
      })
    ()

let validate x = return x

let from_raw global_properties global_assignments raw =
  let+ unchecked =
    from_raw_unchecked global_properties global_assignments raw
  in
  validate unchecked
(* retourne un ok/error selon résultat sur un puzzle, surchargé avec éventuels warnings *)

(* Doit retourner un PuzzleResult *)
let from_chn chn =
  let raw_global_rules, raw_puzzles = RawPuzzle.from_chn chn in
  let global_rules = List.map Rule.from_raw raw_global_rules in
  let+ global_properties, global_assignments =
    split_rules PropertySet.empty init_assignments global_rules
  in
  List.map (from_raw global_properties global_assignments) raw_puzzles |> merge
