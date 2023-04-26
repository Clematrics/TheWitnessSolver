open Definition

type t = {
  properties : PropertySet.t;
  width : int;
  height : int;
  layout : nav array array;
  symbols : (symbol * color) array array;
}

val from_chn : in_channel -> t list Log.log_result
val validate : 'a -> 'a Log.log_result
