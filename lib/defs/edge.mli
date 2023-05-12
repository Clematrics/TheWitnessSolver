type t

val edge : Coords.t -> Coords.t -> t

val get : t -> Coords.t * Coords.t

val is_adjacent : Coords.t -> t -> bool

val other_end : t -> Coords.t -> Coords.t

val direction : t -> Coords.t

val compare : t -> t -> int