type t =
  | Hexagon
  | Square
  | Star
  | Shape of Shape.t * Rotation.t
  | AntiShape of Shape.t * Rotation.t
  | Triad
  | Triangle of int

let to_string = function
  | Hexagon -> "Hexagon"
  | Square -> "Square"
  | Star -> "Star"
  | Shape _ -> "Shape"
  | AntiShape _ -> "AntiShape"
  | Triad -> "Triad"
  | Triangle _ -> "Triangle"
