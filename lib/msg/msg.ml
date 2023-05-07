let coords () (x, y) = Printf.sprintf "At line %i column %i:" (y + 1) (x + 1)

let unknown_assignment =
  Printf.sprintf "%a Cannot find an assignment for character %c" coords

let missing_connection xy path offset =
  Printf.sprintf "%a Path '%s' must be connected to the path %s" coords xy path
    offset

let no_connection xy path =
  Printf.sprintf "%a Path '%s' is not connected to anything" coords xy path

let ambiguous_end_connection =
  Printf.sprintf
    "%a Ambiguous connections for the End %i. End can only be connected to one \
     other path"
    coords

let bad_symbol_position xy sym =
  Printf.sprintf "%a The symbol '%s' is neither in a cell nor on a path" coords
    xy sym
