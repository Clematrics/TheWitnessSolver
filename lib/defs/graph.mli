type t

val empty : t
val edges : t -> Edges.t
val points : t -> CoordSet.t
val add_edge : Edge.t -> t -> t
val add_edges : Edge.t list -> t -> t
val add_point : Coord.t -> t -> t

val remove : Coord.t -> t -> t
(** [remove p g] removes the point [p] and all adjacent edges. *)

val adjacent_edges : Coord.t -> t -> Edges.t
(** [adjacent_edges p g] returns the set of edges adjacent to [p] in [g] *)

val fold_points : (Coord.t -> 'a -> 'a) -> 'a -> t -> 'a
(** [fold_points f init g] is the same as [f p_n' (f p_n (... (f p_0 init)))].
    The order in which points are processed is undefined. *)
