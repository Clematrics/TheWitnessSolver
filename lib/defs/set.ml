module type OrderedType = Stdlib.Set.OrderedType

module type S = sig
  include Stdlib.Set.S

  val pp : (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
end

module Make (O : OrderedType) = struct
  include Stdlib.Set.Make (O)

  let pp pp_elt fmt set =
    let rec pp_elts fmt set =
      match choose_opt set with
      | None -> Format.fprintf fmt ""
      | Some elt ->
          Format.fprintf fmt "%a@ ;@ %a" pp_elt elt pp_elts (remove elt set)
    in
    Format.fprintf fmt "@[<hov 4>{@;%a@;}@]" pp_elts set
end
