type t =
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
  | Custom of bool list list (* TODO: ensure *)

let to_coords =
  let rec fold_char x y coords = function
    | [] -> coords
    | c :: l ->
        fold_char (x + 1) (y + 1) (if c then (x, y) :: coords else coords) l
  and fold_lines y coords = function
    | [] -> coords
    | l :: lines -> fold_lines (y + 1) (fold_char 0 y coords l) lines
  in
  fold_lines 0 []

(* TODO: use custom shapes to define predefined ones *)
let shape = function
  | Block -> [ (0, 0) ]
  | Block4 -> [ (0, 0); (0, 1); (1, 0); (1, 1) ]
  | Bar2 -> [ (0, 0); (1, 0) ]
  | Bar3 -> [ (0, 0); (1, 0); (2, 0) ]
  | Bar4 -> [ (0, 0); (1, 0); (2, 0); (3, 0) ]
  | Corner -> [ (0, 0); (1, 0); (0, 1) ]
  | LPiece -> [ (0, 0); (1, 0); (2, 0); (0, 1) ]
  | JPiece -> [ (0, 0); (1, 0); (0, 1); (0, 2) ]
  | T4Piece -> [ (0, 0); (1, 0); (2, 0); (1, 1) ]
  | T5Piece -> [ (0, 0); (1, 0); (2, 0); (1, 1); (1, 2) ]
  | Diagonal2 -> [ (0, 0); (1, 1) ]
  | Custom b -> to_coords b
