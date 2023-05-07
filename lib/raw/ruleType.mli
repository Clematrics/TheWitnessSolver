open Defs

type t =
  | Property of Property.t
  | Assignment of char * Path.t option * (Symbol.t * Color.t) option
