(** Represent some path semantic as expressed by the user in a raw puzzle file. *)
type t =
  | Meet of bool
      (** [Meet b] is a path that connects to any adjacent paths. [b] indicates
          if the adjacent paths are connected ([b = true]) or not ([b = false]). *)
  | Start of bool
      (** [Start b] is a path that connects to adjacent paths. [b] indicates if
          the start is accessible to the player ([b = true]) or
          disabled/inaccessible ([b = false]). *)
  | End of int
      (** [End i] marks one of the goal of the puzzle. It connects to exactly
          one path, which is the only adjacent path reachable, or the only path
          in the corners if there are no adjacent paths. [i] uniquely identifies
          the end. *)
  | PathHorizontal of bool
      (** [PathHorizontal b] connects two paths horizontally. [b] indicates if
          the path is full ([b = true]) or cut ([b = false]). *)
  | PathVertical of bool
      (** [PathVertical b] connects two paths vertically. [b] indicates if the
          path is full ([b = true]) or cut ([b = false]). *)

open Defs

val get_connections : Coords.t -> t option array array -> Coords.t list
(** [get_connections pos arr] assumes that arr is a 3x3 matrix containing the
    portion of the board restrained around the cell we want the connections.
    [pos] is necessary to give the good conetxt message for potential errors.
    This function logs warnigns and errors, and must be inside the log effect
    handler. *)

val to_string : t -> string
