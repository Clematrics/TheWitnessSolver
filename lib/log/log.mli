(** A logging library based on effects and monadic operations. *)

type log_entry =
  | Context of string
  | Warning of string
  | Error of string
  | Fatal of string

type history = log_entry list
(** The logs, the head being the oldest, and the tail being the last one. *)

type 'a log_result = ('a * history, history) result
(** The result of a logged operation. Returns [Ok(v, h)] if the computation
    succeeded and returned the value [v] with the log [h]. Returns [Error(h)] if
    the computation returned errors. *)

val return : 'a -> 'a log_result
(** Converts a value to a logged value. Monadic return operation. *)

val bind : 'a log_result -> ('a -> 'b log_result) -> 'b log_result
(** [bind x f] applies a logging computation [f] to a logged value [x] and
    returns the result, concatenating the logs. Monadic bind operation. *)

val ( let+ ) : 'a log_result -> ('a -> 'b log_result) -> 'b log_result
(** Convenient binding operator for the [bind] monadic operation. *)

val map : 'a log_result -> ('a -> 'b) -> 'b log_result
(** [map x f] returns [Ok(f v, h)] when [x = Ok(v, h)], [x] otherwise. Logs in
    [f] are not registered. *)

val ( let* ) : 'a log_result -> ('a -> 'b) -> 'b log_result
(** Convenient binding operator for the [map] applicative operation. *)

val merge : 'a log_result list -> 'a list log_result
(** [merge l] merges a list of logged values into a single log, eventually
    containing the list of computed values and the concatenated log history. *)

val log : ('a -> 'b) -> 'a -> 'b log_result
(** [log f x] embeds the computation of [f x] into a logging scope. A call to [perform], [error] or [fatal] during the computation of [f x] will be registered.
    This can be used in the following way:
    {[
        let+ y = log f x in
        y + 1
    ]} or {[
        let%log y = f x in
        y + 1
    ]} if you use the [log_ppx] library, which is equivalent to {[
        let + y = log (fun () -> f x) () in
        y + 1
    ]} *)

val warn : string -> unit
(** Log a warning. This only adds a log to the log history and has no effect on
    the value computed. *)

val error : string -> unit
(** Log an error. The result will be an [Error _], but the computation will not
    fail until the end of the computation is reached. Useful to track all
    possible errors in an iterative computation, for instance when doing an
    operation on each element of a list where each operation can fail
    individually. All operations will be tried and logged until the whole list
    is processed. *)

val fatal : string -> unit
(** Log a fatal error. Stops immediately the computation and generates an
    [Error _]. This is equivalent to an exception. *)

val propagate : history -> unit
(** Propagates an history to an upper effect handler *)
