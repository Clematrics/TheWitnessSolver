type t = Coord.t * Coord.t

let edge c c' = if c' < c then (c', c) else (c, c')
let get x = x
let is_adjacent p ((c, c') : t) = p = c || p = c'

let other_end ((c, c') : t) p =
  if c = p then c'
  else if c' = p then c
  else raise (Invalid_argument "The coordinates are not adjacent to this edge")

let direction (c, c') = Coord.(c' +: ( -: ) c)
(* let normalize (x, y) =
     let rec pgcd x y = if y = 0 then x else pgcd y (x mod y) in
     if x = 0 then (0, 1)
     else if y = 0 then (1, 0)
     else
       let d = if x >= y then pgcd x y else pgcd y x in
       (x / d, y / d)
   in
   let x, y = Coord.(normalize c' +: ( -: ) (normalize c)) in
   if x = 0 then (x, abs y) else (abs x, y) *)

let aligned (c, c') (d, d') =
  let open Coord in
  let x, y = c' +: ( -: ) c and x', y' = d' +: ( -: ) d in
  let coeff a b =
    if a = 0 && b = 0 then Some None
    else if a = 0 || b == 0 then None
    else Some (Some (float_of_int a /. float_of_int b))
  in
  (* None -> No multiple
     Some None -> All possible multiples (both 0)
     Some Some (coeff) -> coeff
  *)
  match (coeff y y', coeff x x') with
  | Some (Some coeff), Some (Some coeff') -> coeff -. coeff' < Float.epsilon
  | Some _, Some None | Some None, Some _ -> true
  | _, _ -> false

let compare = compare

let pp fmt e =
  let p, p' = get e in
  Format.fprintf fmt "(%a -> %a)" Coord.pp p Coord.pp p'
