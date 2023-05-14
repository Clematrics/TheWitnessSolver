open Defs
open Log
open Coords

type t =
  | Meet of bool
  | Start of bool
  | End of int
  | PathHorizontal of bool
  | PathVertical of bool

let to_string = function
  | Meet true -> "Meet"
  | Meet false -> "Meet:Cut"
  | Start _ -> "Start"
  | End i -> Printf.sprintf "End %i" i
  | PathHorizontal false -> "PathHorizontal:Cut"
  | PathHorizontal true -> "PathHorizontal"
  | PathVertical false -> "PathVertical:Cut"
  | PathVertical true -> "PathVertical"

(* Specific to End paths : if only one adjacent path, connects exclusively the End to it.
   If there are no adjacent paths and only one path connected at a corner, connects to it.
   Otherwise, connections are ambiguous, logs an error. *)
let select_connections pos path connections =
  match path with
  | End i -> (
      let adjacent =
        List.filter (Fun.flip List.mem Coords.adjacent) connections
        (* Look for a corner *)
      and corners =
        List.filter (Fun.flip List.mem Coords.corners) connections
      in
      match (List.length adjacent, List.length corners) with
      | 0, 0 -> (* empty connections are checked later *) []
      | 0, 1 -> corners
      | 1, _ -> adjacent
      | _ ->
          error (Msg.ambiguous_end_connection pos i);
          [])
  | _ -> connections

let path_connection = function
  | Meet _ -> Coords.adjacent
  | Start _ -> Coords.adjacent
  | End _ -> Coords.all
  | PathHorizontal _ -> Coords.horizontal
  | PathVertical _ -> Coords.vertical

let accepted_connection = function
  | Meet _ -> Coords.all
  | Start _ -> Coords.adjacent
  | End _ -> Coords.all
  | PathHorizontal _ -> Coords.horizontal
  | PathVertical _ -> Coords.vertical

(* Return false if all the possible connections must be satisfied.
   Return true if connections are optional. *)
let optional_connection = function
  | Start _ | Meet _ -> true
  | End _ -> true
  | PathHorizontal _ | PathVertical _ -> false

let get_connections pos zone =
  let path = Option.get zone.(1).(1) in
  let offsets = path_connection path in
  let opt = optional_connection path in
  let connections =
    offsets
    |> List.filter (fun offset ->
           let ax, ay = offset +: (1, 1) in
           let p = zone.(ax).(ay) in
           let mutual_connection =
             p
             |> Option.map accepted_connection
             |> Option.value ~default:[]
             |> List.mem (offset *: -1)
           in
           if (not opt) && not mutual_connection then
             error
               (Msg.missing_connection pos (to_string path)
                  (Coords.to_string offset));
           mutual_connection)
    |> select_connections pos path
  in
  connections
