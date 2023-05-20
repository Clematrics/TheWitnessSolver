type t = Coord.t * Coord.t

let edge c c' = if c' < c then (c', c) else (c, c')
let get x = x
let is_adjacent p ((c, c') : t) = p = c || p = c'

let other_end ((c, c') : t) p =
  if c = p then c'
  else if c' = p then c
  else raise (Invalid_argument "The coordinates are not adjacent to this edge")

let pgcd ~keep_sign x y =
  (* returns the biggest k (in absolute value) such that x = k*_ and y = k*_
     if y is negative, k is negative
     x = 0 or y = 0 <=> returns 0
  *)
  let rec inner high low = if low = 0 then high else inner low (high mod low) in
  let k =
    if abs x < abs y then inner (abs y) (abs x) else inner (abs x) (abs y)
  in
  if y < 0 && not keep_sign then -k else k

let direction ?(directed = false) (c, c') =
  let sign x = if x < 0 && directed then -1 else 1 in
  let x, y = Coord.(c' -: c) in
  if x = 0 && y = 0 then
    raise (Invalid_argument "Null coordinate invalid for direction")
  else if x = 0 then (0, sign y)
  else if y = 0 then (sign x, 0)
  else
    let k = pgcd ~keep_sign:directed x y in
    (x / k, y / k)

let aligned e e' = direction e = direction e'

let pass_through (c, c') pos =
  if pos = c || pos = c' then true
  else
    let d = direction ~directed:true (c, pos)
    and d' = direction ~directed:true (pos, c') in
    d = d'

let compare = compare

let pp fmt e =
  let p, p' = get e in
  Format.fprintf fmt "(%a -> %a)" Coord.pp p Coord.pp p'
