type t = Coords.t * Coords.t

let edge c c' =
  if c' < c then c',c else c, c'

let get x = x

let compare = compare