(** Shape made of small blocks *)
type t =
  (* TODO: Add other known shapes & custom ones *)
  | Block
  | Bar2
  | Bar3
  | Bar4
  | Corner
  | LPiece
  | JPiece
  | T4Piece
  | T5Piece
  | Block4
  | Diagonal2
  | Custom of bool list list

val shape : t -> Coords.t list
