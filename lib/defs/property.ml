type t =
  | VerticalSymmetry
  | HorizontalSymmetry
  | AxialSymmetry
  | BlueYellowPath
  | Cylindrical

let mapInt = function
  | VerticalSymmetry -> 0
  | HorizontalSymmetry -> 1
  | AxialSymmetry -> 2
  | BlueYellowPath -> 3
  | Cylindrical -> 4

let compare x y = Int.compare (mapInt x) (mapInt y)
