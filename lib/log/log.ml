(*
   Idea:
   - a warning is just a message to the user. The program will proceed anyway
   - an error will continue the operations on the puzzle until the end of the validation
     afterward, the puzzle will be unusable
   - a fatal error will abort all operations on the current puzzle or all puzzles
     -can be implemented by a simple exception depending on where the catch is
*)

open Effect
open Effect.Shallow

type _ Effect.t +=
  | WarningEff : string -> unit t
  | ErrorEff : string -> unit t
  | FatalEff : string -> unit t

type log_entry =
  | Context of string
  | Warning of string
  | Error of string
  | Fatal of string

type history = log_entry list
type log = { history : history; has_errors : bool }
type 'a log_result = ('a * history, history) result

exception Fatal_log of history

let return x : 'a log_result = Ok (x, [])

let bind (r : 'a log_result) (f : 'a -> 'b log_result) : 'b log_result =
  match r with
  | Ok (x, h) -> (
      match f x with Ok (y, h') -> Ok (y, h @ h') | Error h' -> Error (h @ h'))
  | Error e -> Error e

let ( let+ ) = bind

let map (r : 'a log_result) (f : 'a -> 'b) : 'b log_result =
  match r with Ok (x, h) -> Ok (f x, h) | Error h' -> Error h'

let ( let* ) = map

let merge (log_list : 'a log_result list) =
  let res, hist =
    List.fold_left
      (fun (l, (acc_h : history)) -> function
        | Ok (x, h) -> (x :: l, h @ acc_h)
        | Error h -> (l, h @ acc_h))
      ([], []) (List.rev log_list)
  in
  Ok (res, hist)

let rec logging_loop : type a. log -> (a, 'b) continuation -> a -> 'b log_result
    =
 fun log k v ->
  let rec handler =
    {
      retc =
        (fun v ->
          let h = List.rev log.history in
          if log.has_errors then Result.Error h else Result.Ok (v, h));
      exnc = Trax.reraise_with_stack_trace;
      effc =
        (fun (type a) (eff : a t) ->
          match eff with
          | WarningEff s ->
              Some
                (fun (k : (a, _) continuation) ->
                  logging_loop
                    { log with history = Warning s :: log.history }
                    k ())
          | ErrorEff s ->
              Some
                (fun (k : (a, _) continuation) ->
                  logging_loop
                    { history = Error s :: log.history; has_errors = true }
                    k ())
          | FatalEff s ->
              Some
                (fun (k : (a, _) continuation) ->
                  discontinue_with k
                    (Fatal_log (List.rev (Fatal s :: log.history)))
                    handler)
          | _ -> None);
    }
  in
  continue_with k v handler

let log comp = logging_loop { history = []; has_errors = false } (fiber comp)
let warn s = perform (WarningEff s)
let error s = perform (ErrorEff s)
let fatal s = perform (FatalEff s)

let rec propagate = function
  | [] -> ()
  | Context _ :: l -> propagate l (* TODO: *)
  | Warning s :: l ->
      warn s;
      propagate l
  | Error s :: l ->
      error s;
      propagate l
  | Fatal s :: l ->
      fatal s;
      propagate l
