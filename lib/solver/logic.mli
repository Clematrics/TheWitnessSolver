type path_kind = NoPath | Player | Symmetric
type path_index = int
type path = path_kind * path_index
type _ ty = KindTy : path_kind ty | IntTy : path_index ty | PathTy : path ty

type _ var =
  | BoolVariable : string -> bool var
  | PathVariable : string -> path var

(* Quantifier types *)
type _ quty = ..

type _ expr =
  (* Path kinds *)
  | NoPath : path_kind expr
  | Player : path_kind expr
  | Symmetric : path_kind expr
  (* Index expressions *)
  | Add : int expr list -> int expr
  | Sub : int expr * int expr -> int expr
  | Int : path_index -> path_index expr
  (* Path expressions *)
  | Path : path_kind expr * path_index expr -> path expr
  | KindOf : path expr -> path_kind expr
  | IndexOf : path expr -> path_index expr
  | Var : path var -> path expr
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
val evaluate_quantifier : 'a list -> ('a -> 'a list -> 'b) -> 'b list
