open Defs

val unknown_assignment : Coords.t -> char -> string
val missing_connection : Coords.t -> string -> string -> string
val no_connection : Coords.t -> string -> string
val ambiguous_end_connection : Coords.t -> int -> string
val bad_symbol_position : Coords.t -> string -> string
val no_start : string
val no_end : string
val similar_ends : string
val vertical_symmetry_unsatisfied : string
val horizontal_symmetry_unsatisfied : string
val axial_symmetry_unsatisfied : string
val cylindrical_property_unsatisfied : string
val incompatible_symmetry_properties : string -> string -> string
val missing_symmetry : string
val unsymmetric : string -> Coords.t -> Coords.t -> string
