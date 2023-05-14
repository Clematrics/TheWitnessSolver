type t

val edge : Coords.t -> Coords.t -> t
val get : t -> Coords.t * Coords.t
val is_adjacent : Coords.t -> t -> bool
val other_end : t -> Coords.t -> Coords.t

(* TODO: fix *)
val direction : t -> Coords.t
val aligned : t -> t -> bool
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
