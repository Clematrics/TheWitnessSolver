open TheWitness

let usage_msg = "solver <file1> [<file2>] ..."
let input_files = ref []
let anon_fun filename = input_files := filename :: !input_files
let speclist = []

let print_log =
  List.iter (function
    | Log.Context s -> Printf.printf "%s\n" s
    | Log.Warning s -> Printf.printf "   Warning: %s\n" s
    | Log.Error s -> Printf.printf "   Error: %s\n" s
    | Log.Fatal s -> Printf.printf "   Fatal: %s\n" s)

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
        puzzles
        |> List.map (fun p ->
               Printf.printf "Puzzle: %s --------------------------\n"
                 Puzzle.(p.name);
               (p, Logic.solve p))
        |> List.iter (fun (p, solution) ->
               let prefix_path = "output/solved_" in
               Render.render ~prefix_path ~solution (new Render.Style.style) p;
               Render.render (new Render.Style.style) p))
      !input_files
  with e -> Trax.wrap_with_stack_trace e |> Trax.print stderr
