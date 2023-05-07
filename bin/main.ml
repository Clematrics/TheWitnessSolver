open TheWitness

let usage_msg = "solver <file1> [<file2>] ..."
let input_files = ref []
let anon_fun filename = input_files := filename :: !input_files
let speclist = []
let print_log = List.iter (Printf.printf "%s\n")

let () =
  try
    Arg.parse speclist anon_fun usage_msg;
    List.iter
      (fun filename ->
        let filepath = "puzzles/" ^ filename in
        let chn = open_in filepath in
        let puzzles =
          match Puzzle.from_chn chn with
          | Ok (res, log) ->
              print_log log;
              res
          | Error log ->
              print_log log;
              []
        in
        List.iter (Render.render (new Render.Style.style)) puzzles)
      !input_files
  with e -> Trax.wrap_with_stack_trace e |> Trax.print stderr
