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

let no_start = "No start path present, there should be at least one"
let no_end = "No end path present, there should be at least one"
let similar_ends = "Multiple ends are sharing the same id."

let vertical_symmetry_unsatisfied =
  "The property VerticalSymmetry is unsatisfied."

let horizontal_symmetry_unsatisfied =
  "The property HorizontalSymmetry is unsatisfied."

let axial_symmetry_unsatisfied = "The property AxialSymmetry is unsatisfied."

let cylindrical_property_unsatisfied =
  "The property Cylindrical is unsatisfied."

let incompatible_symmetry_properties a b =
  Printf.sprintf "Property %s cannot occur with %s" a b

let missing_symmetry =
  "A symmetry property must be present to satisfy the property BlueYellowPath."

let unsymmetric s c c' =
  Printf.sprintf "%a %a Unsymmetric %s" coords c coords c' s
