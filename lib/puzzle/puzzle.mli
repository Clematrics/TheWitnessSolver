open Defs
module Board : Map.S with type key = Coords.t
module CoordSet : Set.S with type elt = Coords.t
module IntMap : Map.S with type key = Int.t

type path =
  | Path of bool
      (** [Path b] describes a path that can be connected to adjacent path
          elements. The boolean [b] indicates if the path is active ([b=true])
          or not. This can represent all kind of paths : For instance, a cut
          horizontal path is encoded as a [Path false] connected to its left and
          right. *)
  | Start of bool
      (** [Start b] describes a start node. [b=true] when the puzzle can be
          started from this start, [b=false] otherwise. This is useful to
          represent specific puzzles where a start is hidden or unavailable. *)
  | End of int
      (** [End i] represents a possible objective of a puzzle, identified with
          the integer [i] *)

type element = {
  path : path option;
  symbol : (Symbol.t * Color.t) option;
  connected_paths : Coords.t list;
  connected_cells : Coords.t list;
}

type t = {
  name : string;
  properties : PropertySet.t;
  width : int;
  height : int;
  board : element Board.t;
  paths : CoordSet.t;  (** A set of coordinates indicating where paths intersections are *)
  edges : Edges.t;
  starts : bool Board.t;  (** A (coordinates -> activated) map of starts *)
  ends : Coords.t IntMap.t;  (** A (identifier -> coordinates) map of ends *)
}

val from_chn : in_channel -> t list Log.log_result
