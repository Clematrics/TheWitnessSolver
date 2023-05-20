open Defs
module EdgesVar : Map.S
module PosSet = CoordSet
module PosVar = CoordMap

open Logic

type context = {
  junctions : path var PosVar.t;
  activated : bool var PosVar.t;
  (* Starts (usable by the player) & Ends positions *)
  starts : PosSet.t;
  ends : PosSet.t;
  (* symbols *)
  hexagons : Color.t PosVar.t;
}

type t = {
  context: context;
  assertions: bool expr list
}

val logic_problem_of : Puzzle.t -> t