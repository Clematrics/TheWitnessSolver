type t = SxPy | SxNy | PxPy | NxNy | PxSy | NxSy | NxPy | PxNy

val opposite : t -> t

val map_coords : t -> Coords.t

val compare : t -> t -> int