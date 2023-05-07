type t =
  | VerticalSymmetry
  | HorizontalSymmetry
  | AxialSymmetry
  | BlueYellowPath
  | Cylindrical

val compare : t -> t -> int
