type t

val edge : Coord.t -> Coord.t -> t
val get : t -> Coord.t * Coord.t
val is_adjacent : Coord.t -> t -> bool
val other_end : t -> Coord.t -> Coord.t

val direction : ?directed:bool -> t -> Coord.t
val aligned : t -> t -> bool
val pass_through : t -> Coord.t -> bool
val compare : t -> t -> int
val pp : Format.formatter -> t -> unit
