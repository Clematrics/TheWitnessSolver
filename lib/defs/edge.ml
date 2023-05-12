type t = Coords.t * Coords.t

let edge c c' = if c' < c then (c', c) else (c, c')
let get x = x
let is_adjacent p ((c, c') : t) = p = c || p = c'

let other_end ((c, c') : t) p =
  if c = p then c'
  else if c' = p then c
  else raise (Invalid_argument "The coordinates are not adjacent to this edge")

let direction (c, c') = Coords.(c' +: ( -: ) c)
let compare = compare
