type t

val edge : Coords.t -> Coords.t -> t

val get : t -> Coords.t * Coords.t

val compare : t -> t -> int