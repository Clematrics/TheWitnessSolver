open Defs
open Log

type t = {
  name : string;
  properties : PropertySet.t;
  width: int;
  height: int;
  paths : Path.t option array array;
  symbols : (Symbol.t * Color.t) option array array;
}

val from_chn : in_channel -> t list log_result
(** [from_chn chn] reads a set of puzzles from a channel and returns them.

    @param [chn] channel to read from
    @return the list of raw puzzles *)

module Path : module type of Path