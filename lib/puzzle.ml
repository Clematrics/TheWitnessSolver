type property =
  | VerticalSymmetry
  | HorizontalSymmetry
  | AxialSymmetry
  | BlueYellowPath
  | Cylindrical

module PropertySet = Set.Make (struct
  type t = property

  let mapInt = function
    | VerticalSymmetry -> 0
    | HorizontalSymmetry -> 1
    | AxialSymmetry -> 2
    | BlueYellowPath -> 3
    | Cylindrical -> 4

  let compare x y = Int.compare (mapInt x) (mapInt y)
end)

type rawPuzzle = { rules : string list; lines : string list }

type nav =
  | EmptyNav
  | Meet
  | Start
  | End of int
  | PathHorizontal
  | PathVertical
  | CutPathHorizontal
  | CutPathVertical

(** All combinations of red, green and blue, and a wildcard color *)
type color =
  | Any
  | Red
  | Green
  | Blue
  | White
  | Cyan
  | Magenta
  | Yellow
  | Black

(** CornerXX where XX indicates the position of the corner
    TL for top left
    BR for bottom right
    …
    Any for a wildcard rotation
    Same thing for the L shape
*)
type shape =
  | Unit
  | TwoBarHorizontal
  | TwoBarVertical
  | TwoBarAny
  | ThreeBarHorizontal
  | ThreeBarVertical
  | ThreeBarAny
  | FourBarHorizontal
  | FourBarVertical
  | FourBarAny
  | CornerTL
  | CornerTR
  | CornerBL
  | CornerBR
  | CornerAny
  | LTL
  | LTR
  | LBL
  | LBR
  | LAny
  | LReversedTL
  | LReversedTR
  | LReversedBL
  | LReversedBR
  | LReversedAny

type symbol =
  | EmptySymbol
  | Hexagon
  | Square
  | Star
  | Shape of shape
  | Triad
      (** The shape with three branches which cancel another connected shape *)

type rule =
  | Property of property
  | Assignment of char * nav option * (symbol * color) option

type puzzle = {
  properties : PropertySet.t;
  width : int;
  height : int;
  layout : nav array array;
  symbols : (symbol * color) array array;
}
