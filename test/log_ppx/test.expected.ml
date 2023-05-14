let () =
  let+ x, y = Log.log (fun () -> split) () in
  x
