type nav =
  | Meet
  | Start
  | End of int
  | PathHorizontal
  | PathVertical
  | CutPathHorizontal
  | CutPathVertical

(** All combinations of red, green and blue, and a wildcard color *)
type color =
  | AnyColor
  | Red
  | Green
  | Blue
  | White
  | Cyan
  | Magenta
  | Yellow
  | Black
  | Orange

(** Shape made of small blocks *)
type shape =
(* TODO: Add other known shapes & custom ones *)
  | Block
  | Bar2
  | Bar3
  | Bar4
  | Corner
  | L
  | LMirrored
  | T
  | SmallT
  | Block4
  | Diagonal2

(** Rotation of shapes, trigonometric way *)
type rotation =
  | NoRotation
  | Rotation90
  | Rotation180
  | Rotation270
  | AnyRotation

type symbol =
  | Hexagon
  | Square
  | Star
  | Shape of shape * rotation
  | AntiShape of shape * rotation
  | Triad
  | Triangle of int
      (** The shape with three branches which cancel another connected shape *)
