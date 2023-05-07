type t = int * int

val compare : t -> t -> int
val ( +: ) : t -> t -> t
val ( *: ) : t -> int -> t
val ( -: ) : t -> t

val all : t list
val adjacent : t list
val horizontal : t list
val vertical : t list
val corners : t list

val to_string : t -> string