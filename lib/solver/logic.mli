type path_kind = NoPath | Player | Symmetric
type path_index = int
type zone = int
type path = path_kind * path_index
type cell = Defs.Coord.t
type symbol = |

type _ ty =
  | KindTy : path_kind ty
  | IntTy : int ty
  | PathTy : path ty
  | SymbolTy : symbol ty

type _ var =
  | BoolVariable : string -> bool var
  | PathVariable : string -> path var
  | ZoneVariable : string -> zone var
  | CellVariable : string -> cell var

(* Quantifier types *)
type _ quty = ..

type _ expr =
  (* Path kinds *)
  | NoPath : path_kind expr
  | Player : path_kind expr
  | Symmetric : path_kind expr
  (* Index & Int expressions *)
  | Int : int -> int expr
  | Add : int expr list -> int expr
  | Sub : int expr * int expr -> int expr
  | Less : int expr * int expr -> bool expr
  (* Path expressions *)
  | Path : path_kind expr * path_index expr -> path expr
  | KindOf : path expr -> path_kind expr
  | IndexOf : path expr -> path_index expr
  | Var : path var -> path expr
  (* Cells & cell zone expressions *)
  | Zone : zone var -> int expr
  (* Other expressions *)
  | IfThenElse : bool expr * 'a expr * 'a expr -> 'a expr
  (* Boolean formulas *)
  | False : bool expr
  | True : bool expr
  | Bool : bool var -> bool expr
  | NotEqual : 'a ty * 'a expr * 'a expr -> bool expr
  | Equal : 'a ty * 'a expr * 'a expr -> bool expr
  | And : bool expr list -> bool expr
  | Or : bool expr list -> bool expr
  | Xor : bool expr * bool expr -> bool expr
  | Imply : bool expr * bool expr -> bool expr
  | Equiv : bool expr * bool expr -> bool expr
  | Not : bool expr -> bool expr
  (* Symbols expressions *)
  | LinkedSymbolOf : Defs.Coord.t -> symbol expr
      (** The symbol pointed by the other symbol at those coordinates.
          It is used to represent the connection between a triad and another symbol it satisfies,
          or by star symbols to link to their counterpart. *)
  | SymbolOf : Defs.Coord.t -> symbol expr
      (** The unique symbol constructor attached to the symbol located at the
          position provided *)
  (* Relations *)
  | Neighbor : cell * cell -> bool expr
  | Connected : cell * cell -> bool expr
  (* Quantifiers *)
  | Exists : 'a quty * 'a list * ('a -> 'a list -> bool expr) -> bool expr
  | Forall : 'a quty * 'a list * ('a -> 'a list -> bool expr) -> bool expr

val ( <=> ) : bool expr -> bool expr -> bool expr
val ( ==> ) : bool expr -> bool expr -> bool expr
val ( &&& ) : bool expr -> bool expr -> bool expr
val ( ||| ) : bool expr -> bool expr -> bool expr
val ( === ) : 'a expr -> 'a expr -> 'a ty -> bool expr
val ( =!= ) : 'a expr -> 'a expr -> 'a ty -> bool expr
val ( +++ ) : path_index expr -> path_index expr -> path_index expr
val ( --- ) : path_index expr -> path_index expr -> path_index expr
val ( ++ ) : 'a list -> 'a -> 'a list
val ( ++? ) : 'a list -> 'a option -> 'a list
val ( >>> ) : int expr -> int expr -> bool expr
val ( <<< ) : int expr -> int expr -> bool expr
val evaluate_quantifier : 'a list -> ('a -> 'a list -> 'b) -> 'b list
val pp_int : Format.formatter -> int expr -> unit
val pp_kind : Format.formatter -> path_kind expr -> unit
val pp_path : Format.formatter -> path expr -> unit
val pp : Format.formatter -> bool expr -> unit
