open Log
open Defs
open ParserTypes
open Assignment

type t = {
  name : string;
  properties : PropertySet.t;
  width : int;
  height : int;
  paths : Path.t option array array;
  symbols : (Symbol.t * Color.t) option array array;
}

(** Helper function to trim a [value] from a [list] at the beginning and the
    end. [trim 0 \[0; 0; 1; 1; 0; 1; 0\]] returns [\[1; 1; 0; 1\]]
    [trim 0 \[1; 0; 0\]] returns [\[1\]] [trim 0 \[1; 1\] returns \[1; 1\]]

    @param value the value to trim in the list
    @param list to trim
    @return the trimmed list *)
let list_trim value list =
  let rec trim_start = function
    | [] -> []
    | x :: l when x = value -> trim_start l
    | l (* x != value *) -> l
  in
  trim_start list |> List.rev |> trim_start |> List.rev

(* Split rules into their respective categories
   *)
let merge_rules init rules =
  let properties, assignments =
    List.fold_left
      (fun (set, ass) -> function
        | Rule.Property x -> (PropertySet.add x set, ass)
        | Rule.Assignment (c, nav, sym) -> (set, Map.add c (nav, sym) ass))
      init rules
  in
  (properties, assignments)

let matrix_view (width, height) assignments lines =
  let layout = Array.make_matrix width height None in
  let symbols = Array.make_matrix width height None in
  lines
  |> List.iteri (fun y line ->
         line
         |> String.iteri (fun x c ->
                let nav, sym =
                  try Map.find c assignments
                  with Not_found ->
                    error (Msg.unknown_assignment (x, y) c);
                    (None, None)
                in
                layout.(x).(y) <- nav;
                symbols.(x).(y) <- sym));
  (layout, symbols)

let get_dimension lines =
  let rec count width height = function
    | [] -> (width, height)
    | str :: l' -> count (max (String.length str) width) (height + 1) l'
  in
  count 0 0 lines

let from_chn chn =
  let raw_lex_buf = Lexing.from_channel chn in
  let raw_global_rules, raw_puzzles = Parser.parse Lexer.lex raw_lex_buf in
  (* converting global rules *)
  let%log global_rules =
    raw_global_rules |> List.map Rule.from_raw
    |> merge_rules (PropertySet.empty, init_assignments)
  in
  raw_puzzles
  |> List.map (fun raw ->
         (* converting local rules *)
         let%log properties, assignments =
           raw.rules |> List.map Rule.from_raw |> merge_rules global_rules
         in
         (* trimming empty rows at the start and end*)
         let lines = list_trim "" raw.lines in
         let ((width, height) as dim) = get_dimension lines in
         (* applying assignments to get a matrix view *)
         let%log paths, symbols = matrix_view dim assignments lines in
         return { name = raw.name; properties; width; height; paths; symbols })
  |> merge

module Path = Path
