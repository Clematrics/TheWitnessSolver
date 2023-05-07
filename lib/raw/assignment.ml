(* module type MAP = Map.S with type key = Char.t *)
module Map = Map.Make(Char)

open Defs

type t = (Path.t option * (Symbol.t * Color.t) option) Map.t

(* Default assignments:
   - the space character is replaced by nothing
*)
let init_assignments : t = Map.add ' ' (None, None) Map.empty