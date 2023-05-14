type t = int * int

let compare = compare
let ( +: ) (x, y) (x', y') = (x + x', y + y')
let ( *: ) (x, y) n = (n * x, n * y)
let ( -: ) (x, y) = (-x, -y)

let all =
  [ (0, -1); (0, 1); (-1, -1); (1, 1); (-1, 0); (1, 0); (1, -1); (-1, 1) ]

let adjacent = [ (0, -1); (0, 1); (-1, 0); (1, 0) ]
let horizontal = [ (-1, 0); (1, 0) ]
let vertical = [ (0, -1); (0, 1) ]
let corners = [ (-1, -1); (1, 1); (1, -1); (-1, 1) ]
let to_float (x, y) = (float_of_int x, float_of_int y)

let to_string = function
  | 0, -1 -> "above"
  | 0, 1 -> "below"
  | -1, -1 -> "to its top left corner"
  | 1, 1 -> "to its bottom right corner"
  | -1, 0 -> "to its left"
  | 1, 0 -> "to its right"
  | 1, -1 -> "to its top right corner"
  | -1, 1 -> "to its bottom left corner"
  | x, y -> Printf.sprintf "at relative coords (%i, %i)" x y

let pp fmt (x, y) = Format.fprintf fmt "%i, %i" x y
