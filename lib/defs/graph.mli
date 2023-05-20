(** It is garuanteed that both ends of an edge are points contained in the
    graph. *)

type t

val empty : t
val edges : t -> Edges.t
val points : t -> CoordSet.t

val add_edge : Edge.t -> t -> t
(** [add_edge e g] adds the edge [e] and the ends [p] and [p'] of [e] to [g]. *)

val add_edges : Edge.t list -> t -> t
(** [add_edges edges g] is equivalent to
    [List.fold_left (Fun.flip add_edge) g edges]. *)

val add_point : Coord.t -> t -> t

val remove : Coord.t -> t -> t
(** [remove p g] removes the point [p] and all adjacent edges. *)

val remove_edge : Edge.t -> t -> t
(** [remove e g] removes the edge [e]. If the points adjacent to [e] have an
    arity of 0 after the edge is removed, they are removed too. *)

val mem : t -> Coord.t -> bool
(** [mem g p] returns [true] if the point [p] is in [g], and [false] otherwise. *)

val adjacent_edges : Coord.t -> t -> Edges.t
(** [adjacent_edges p g] returns the set of edges adjacent to [p] in [g]. If the
    point [p] is not in the graph, [Edges.empty] is returned. *)

val edge_through : Coord.t -> t -> Edge.t option
(** [edge_through p g] returns [Some e] if there exists an edge [e] in [g] which
    [p] passes through and [None] if there are no such edge. *)

val arity : Coord.t -> t -> int
(** [arity p g] returns the aroty of the point [p] in the graph [g], ie. the
    number of edges adjacent to [p]. If [p] is not in [g], 0 is returned. This
    is equivalent to [Edges.cardinal (adjacent_edges p g)] *)

val arity_map : t -> int CoordMap.t
(** [arity_map g] returns the map of points to integers, where the binding (p,
    i) is in the map if and only if [p] is in the graph [g] with arity [i]. *)

val fold_points : (Coord.t -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold_points f g init] is the same as [f p_n' (f p_n (... (f p_0 init)))].
    The order in which points are processed is undefined. *)

val fold_edges : (Edge.t -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold_edges f g init] is the same as [f e_n' (f e_n (... (f e_0 init)))].
    The order in which edges are processed is undefined. *)

val union : t -> t -> t
(** [union g g'] merges two graphs together, by merging their points and edges. *)

val pp : Format.formatter -> t -> unit
