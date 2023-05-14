module Map : Map.S with type key = Char.t
open Defs

type t = (Path.t option * (Symbol.t * Color.t) option) Map.t

val init_assignments : t
(** Default assignments:

    - the space character is replaced by nothing *)
