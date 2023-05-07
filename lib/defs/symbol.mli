type t =
  | Hexagon
  | Square
  | Star
  | Shape of Shape.t * Rotation.t
  | AntiShape of Shape.t * Rotation.t
  | Triad
  | Triangle of int
      (** The shape with three branches which cancel another connected shape *)

val to_string : t -> string