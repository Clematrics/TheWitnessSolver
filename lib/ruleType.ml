open Definition

type t =
  | Property of Property.t
  | Assignment of char * nav option * (symbol * color) option
