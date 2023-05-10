open Defs
module CoordSet : Set.S with type elt = Coords.t
module CoordMap : Map.S with type key = Coords.t
module IntMap : Map.S with type key = Int.t

(* TODO: deprecate *)
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

(* TODO: deprecate *)
type element = {
  path : path option;
  symbol : (Symbol.t * Color.t) option;
  connected_paths : Coords.t list;
  connected_cells : Coords.t list;
}

type t = {
  name : string;
      (** The name of the puzzle. TODO: To replace with a full reference: file +
          name *)
  properties : PropertySet.t;  (** The set of properties of the puzzle *)
  width : int;
      (** The width of the puzzle. All elements are guaranted to have their x
          coordinate in [0, width - 1] *)
  height : int;
      (** The height of the puzzle. All elements are guaranted to have their y
          coordinate in [0, height - 1] *)
  paths : bool CoordMap.t;
      (** A (coordinates -> boolean) map describing where path intersections are
          and if they are usable. Be careful, if a coordinate is in [paths] and
          mapped to false, it means that their IS a path at this coordinate, but
          it is not usable by the player or by any symmetric path (for instance
          when there is a PathHorizontal:Cut). It is encoded this way to
          simplify cell detections.

          TODO: is it really necessary? Yes if we eventually want to guarantee
          that both sides of edges are existing paths.

          Connections between paths are not described by this field, but by
          {!field:edges}. *)
  edges : Edges.t;
      (** The set of non-directional edges linking two paths together. An edge
          can exist in this set even if it leads to a dead end. For instance, a
          PathHorizontal:Cut will generate two edges between the center and the
          extremities. *)
  starts : bool CoordMap.t;
      (** A (coordinates -> boolean) map of starts. If a coordinate is mapped to
          false, it means that this start is not usable by the player, either
          because it is blocked or visally obstructed. *)
  ends : Coords.t IntMap.t;
      (** A partially bijective (identifier -> coordinates) map of ends *)
  cells : CoordSet.t;  (** The set of cells, characterised by their center *)
  symbols : (Symbol.t * Color.t) CoordMap.t
}
(** The type describing a puzzle

    A puzzle is described by a grid of elements on the plane of natural
    coordinates (x, y) ∈ ℕ²

    {[
      ┌─────→ x
      │
      │
      ↓
      y
    ]} *)

val from_chn : in_channel -> t list Log.log_result
