open Definition

module Board : Map.S with type key = Coords.t

type element = {
  path : nav option;
  symbol : (symbol * color) option;
  connected_paths : OffsetSet.t;
  connected_cells : OffsetSet.t;
}


type t = {
  properties : PropertySet.t;
  width : int;
  height : int;
  board : element Board.t;
}

val from_chn : in_channel -> t list Log.log_result
val validate : 'a -> 'a Log.log_result
