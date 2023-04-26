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

(** CornerXX where XX indicates the position of the corner TL for top left BR
    for bottom right … Any for a wildcard rotation Same thing for the L shape *)
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
