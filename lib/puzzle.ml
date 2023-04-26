open Definition
open RawPuzzle

type t = {
  properties : PropertySet.t;
  width : int;
  height : int;
  layout : nav array array;
  symbols : (symbol * color) array array;
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

let from_raw_unchecked global_properties global_assignments raw =
  let+ properties, assignments =
    split_rules global_properties global_assignments
      (List.map Rule.from_raw raw.rules)
  in
  log
    (fun () ->
      let width, height = RawPuzzle.get_dimension raw in
      let layout = Array.make_matrix height width EmptyNav in
      let symbols = Array.make_matrix height width (EmptySymbol, Any) in
      raw.lines
      |> List.iteri (fun y ->
             String.iteri (fun x c ->
                 let nav, sym =
                   try AssignmentMap.find c assignments
                   with Not_found ->
                     error
                       (Printf.sprintf
                          "Cannot find character %c at line %i column %i" c
                          (y + 1) (x + 1));
                     (None, None)
                 in
                 Option.iter (fun n -> layout.(x).(y) <- n) nav;
                 Option.iter (fun s -> symbols.(x).(y) <- s) sym));
      { properties; width; height; layout; symbols })
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
  return @@ List.map (from_raw global_properties global_assignments) raw_puzzles
