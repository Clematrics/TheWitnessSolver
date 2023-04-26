include RawPuzzleType

(** Helper function to trim a [value] from a [list] at the beginning and the
    end. [trim 0 \[0; 0; 1; 1; 0; 1; 0\]] returns [\[1; 1; 0; 1\]]
    [trim 0 \[1; 0; 0\]] returns [\[1\]] [trim 0 \[1; 1\] returns \[1; 1\]]

    @param value the value to trim in the list
    @param list to trim
    @return the trimmed list *)
let list_trim value list =
  let rec trim_start = function
    | [] -> []
    | x :: l when x == value -> trim_start l
    | _ :: l (* x != value*) -> l
  in
  trim_start list |> List.rev |> trim_start

let from_chn chn : raw_file =
  let raw_lex_buf = Lexing.from_channel chn in
  let raw_global_rules, raw_puzzles =
    RawParser.parse RawLexer.lex raw_lex_buf
  in
  (* trimming empty rows at the start and end*)
  let raw_puzzles =
    List.map (fun p -> { p with lines = list_trim "" p.lines }) raw_puzzles
  in
  (raw_global_rules, raw_puzzles)

let get_dimension { lines; _ } =
  let rec count width height = function
    | [] -> (width, height)
    | str :: l' -> count (max (String.length str) width) (height + 1) l'
  in
  count 0 0 lines
