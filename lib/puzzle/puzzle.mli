open Defs
module IntMap : Map.S with type key = Int.t

type t = {
  file : string;
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
  logic_graph : Graph.t;
      (** The logical graph of the puzzle. It does not include the
          cuts/dead-ends and edges leading to those. This is the optimised
          version of [graph]. *)
  graph : Graph.t;
      (** The visual graph of the puzzle. It does not include the cuts/dead-ends
          and edges leading to those. This is the non-optimised version of
          [logic_graph]. *)
  cuts_graph : Graph.t;
      (** The set of edges and points which are necessary to visualise the
          puzzle as it was described but are only dead-ends and cuts for the
          logical puzzle. *)
  starts : bool CoordMap.t;
      (** A (coordinates -> boolean) map of starts. If a coordinate is mapped to
          false, it means that this start is not usable by the player, either
          because it is blocked or visally obstructed. *)
  ends : Coord.t IntMap.t;
      (** A partially bijective (identifier -> coordinates) map of ends *)
  cells : CoordSet.t;  (** The set of cells, characterised by their center *)
  symbols : (Symbol.t * Color.t) CoordMap.t;
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

val from_chn : ?filename:string -> in_channel -> t list Log.log_result
