open Defs
module EdgesVar : Map.S
module PosSet = CoordSet
module PosVar = CoordMap
open Logic

type context = {
  junctions : path var PosVar.t;
  edges : bool var EdgesVar.t;
  (* Starts (usable by the player) & Ends positions *)
  starts : PosSet.t;
  ends : PosSet.t;
  (* cells *)
  cells_id : int PosVar.t;
  cells_zone : zone var PosVar.t;
  (* symbols *)
  activated : bool var PosVar.t;
  hexagons : Color.t PosVar.t;
}

type t = { context : context; assertions : bool expr list }

val logic_problem_of : Puzzle.t -> t
