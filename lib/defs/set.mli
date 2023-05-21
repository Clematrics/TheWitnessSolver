module type OrderedType = Stdlib.Set.OrderedType

module type S = sig
  include Stdlib.Set.S

  val pp : (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
end

module Make (O : OrderedType) : S with type elt = O.t
