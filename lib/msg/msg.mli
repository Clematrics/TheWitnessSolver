open Defs

val unknown_assignment : Coords.t -> char -> string
val missing_connection : Coords.t -> string -> string -> string
val no_connection : Coords.t -> string -> string
val ambiguous_end_connection : Coords.t -> int -> string
val bad_symbol_position : Coords.t -> string -> string
