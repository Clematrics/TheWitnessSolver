open Gg
open Vg
open Defs
open Puzzle

let of_color =
  let open Defs.Color in
  function
  | AnyColor -> Gg.Color.v_srgbi 42 42 42
  | Red -> Gg.Color.v_srgbi 255 0 0
  | Green -> Gg.Color.v_srgbi 0 255 0
  | Blue -> Gg.Color.v_srgbi 0 0 255
  | White -> Gg.Color.v_srgbi 255 255 255
  | Cyan -> Gg.Color.v_srgbi 0 255 255
  | Magenta -> Gg.Color.v_srgbi 255 0 255
  | Yellow -> Gg.Color.v_srgbi 255 255 0
  | Black -> Gg.Color.v_srgbi 0 0 0
  | Orange -> Gg.Color.v_srgbi 255 128 0

let path_width = 0.5
let cell_size = 2.
let cell_inner = cell_size -. path_width
let v2 p = p |> Coords.to_float |> V2.of_tuple

let path_from_points ?(rel = false) = function
  | [] -> P.empty
  | p :: l -> List.fold_left (Fun.flip (P.line ~rel)) (P.sub ~rel p P.empty) l

let shape_from_points ?(rel = false) p = p |> path_from_points ~rel |> P.close
let points_from_coords = List.map V2.of_tuple

(* Peut-être factorisé *)
(* let render_path style board pos elt = function
   | Path b ->
       (* TODO: *)
       let img =
         P.empty
         |> P.rect
              (Box2.v
                 V2.(zero - (v path_width path_width / 2.))
                 V2.(v path_width path_width))
         |> Fun.flip I.cut (I.const style#navigation_color)
       in
       let img =
         if List.length elt.connected_paths = 2 then
           let x, y =
             List.fold_left
               (fun o -> o |> Coords.( +: ))
               (0, 0) elt.connected_paths
           in
           let c =
             V2.( * ) (path_width /. 2.) (V2.v (float_of_int x) (float_of_int y))
           in
           I.cut (P.empty |> P.circle c path_width) img
         else img
       in
       let img = if b then img else I.void in
       List.fold_left
         (fun img o ->
           match CoordMap.find_opt Coords.(pos +: o) board with
           | Some { path = Some (Path _); _ } | Some { path = Some (Start _); _ }
             ->
               let cell_half_inner = (cell_size /. 2.) -. path_width in
               let dx, dy = o in
               let dx, dy = (float_of_int dx, float_of_int dy) in
               let sx, sy =
                 ( (dx *. cell_half_inner) -. (dy *. path_width),
                   (dy *. cell_half_inner) +. (dx *. path_width) )
               in
               let dx, dy = (dx +. dy, dy -. dx) in
               let path =
                 P.empty
                 |> P.rect
                      (Box2.v
                         V2.(mul (v dx dy) (v path_width path_width / 2.))
                         V2.(v sx sy))
               in
               I.cut path (I.const style#navigation_color) |> I.blend img
           | _ -> img)
         img elt.connected_paths
   | Start b ->
       let circ = P.circle V2.zero path_width P.empty in
       I.cut circ
         (I.const (if b then style#navigation_color else style#disabled_color))
   | End _ ->
       let dx, dy = List.hd elt.connected_paths in
       let path =
         [
           ( cell_size /. 2. *. float_of_int dx,
             cell_size /. 2. *. float_of_int dy );
           ( cell_size /. 4. *. float_of_int dx,
             cell_size /. 4. *. float_of_int dy );
         ]
         |> points_from_coords |> path_from_points
       in
       let area = `O { P.o with width = path_width; cap = `Round } in
       I.cut ~area path (I.const style#navigation_color) *)

let img_from_shape color shape r =
  let bsize = 0.9 in
  let block (i, j) =
    P.rect
      (Box2.v (V2.v (float_of_int i) (float_of_int j)) (V2.v bsize bsize))
      P.empty
  in
  let merge = List.fold_left P.append P.empty in
  let box blocks =
    let max_x, max_y =
      List.fold_left
        (fun (max_x, max_y) (x, y) -> (max max_x x, max max_y y))
        (Int.min_int, Int.min_int) blocks
    and min_x, min_y =
      List.fold_left
        (fun (min_x, min_y) (x, y) -> (min min_x x, min min_y y))
        (Int.max_int, Int.max_int) blocks
    in
    let origin = V2.v (float_of_int min_x) (float_of_int min_y)
    and corner = V2.v (float_of_int (max_x + 1)) (float_of_int (max_y + 1)) in
    Box2.v origin V2.(corner - origin)
  in
  let rotation =
    let open Defs.Rotation in
    function
    | NoRotation -> 0.
    | Rotation90 -> Float.pi /. 2.
    | Rotation180 -> Float.pi
    | Rotation270 -> -.Float.pi /. 2.
    | AnyRotation -> Float.pi /. 6.
  in
  let frame box rotation img =
    let size = max (max (Box2.w box) (Box2.h box)) 5. in
    I.move V2.(zero - Box2.mid box) img
    |> I.rot rotation
    |> I.scale V2.(1. /. size *. cell_size *. 0.8 * (ox + oy))
  in
  let coords = Defs.Shape.shape shape in
  let box = box coords in
  let img =
    coords |> List.map block |> merge |> Fun.flip I.cut (I.const color)
  in
  frame box (rotation r) img

let img_from_triangle style i =
  let h = sqrt 3. /. 2. in
  let t =
    [ (0., 0.); (1., 0.); (0.5, -.h) ]
    |> points_from_coords |> shape_from_points
  in
  let triangle = I.cut t (I.const style#triangle_color) in
  let triangle2 = I.move (V2.v 1.2 0.) triangle in
  let triangle3 = I.move (V2.v 0.6 (-.h -. 0.2)) triangle in
  let img =
    match i with
    | 1 -> triangle |> I.move (V2.v (-0.5) (h /. 2.))
    | 2 -> triangle |> I.blend triangle2 |> I.move (V2.v (-1.1) (h /. 2.))
    | 3 ->
        triangle |> I.blend triangle2 |> I.blend triangle3
        |> I.move (V2.v (-1.1) (h +. 0.1))
    | _ -> assert false
  in
  I.scale (V2.v (1. /. 2.) (1. /. 2.)) img

let render_symbol style color =
  let on_circle r theta = V2.of_polar (V2.v r theta) in
  let angles cnt =
    List.init cnt (fun i ->
        float_of_int i *. 2. *. Float.pi /. float_of_int cnt)
  in
  let open Defs.Symbol in
  function
  | Hexagon ->
      let angles = angles 6 in
      let points = List.map (on_circle 0.1) angles in
      let path = shape_from_points points in
      I.cut path (I.const color)
  | Square ->
      let width = 7. /. 5. *. path_width in
      let round = path_width /. 5. in
      let box_size = V2.v width width in
      let pos = V2.(box_size / -2.) in
      let rect = P.rrect (Box2.v pos box_size) (V2.v round round) P.empty in
      I.cut rect (I.const color)
  | Star ->
      let path =
        angles 16
        |> List.map (on_circle (path_width /. 2.))
        |> List.mapi (fun i p -> if i mod 2 = 0 then V2.(7. /. 5. * p) else p)
        |> shape_from_points
      in
      I.cut path (I.const color)
  | Shape (s, r) -> img_from_shape color s r
  | AntiShape (s, r) -> img_from_shape color s r (* TODO: *)
  | Triangle i -> img_from_triangle style i
  | Triad ->
      let points = angles 3 |> List.map (on_circle (path_width /. 2.)) in
      let path =
        match points with
        | [ p1; p2; p3 ] ->
            let path = path_from_points [ V2.zero; p1 ]
            and path' = path_from_points [ p2; V2.zero; p3 ] in
            P.append path' path
        | _ -> assert false
      in
      let area = `O { P.o with width = 0.1 } in
      let img = I.cut ~area path (I.const color) in
      I.rot (-.Float.pi /. 2.) img

let render_image path size view img =
  try
    let oc = open_out path in
    let r = Vgr.create (Vgr_svg.target ()) (`Channel oc) in
    try
      ignore (Vgr.render r (`Image (size, view, img)));
      ignore (Vgr.render r `End);
      close_out oc
    with e ->
      close_out oc;
      raise e
  with Sys_error e -> prerr_endline e

type incomplete_path = {
  start_by_path : bool;
  completed : Coords.t list;
      (** Accumulator of all points crossed on the path. It is garuanteed to
          have at least two elements at creation *)
  last_edge : Edge.t;
}

type completed_path = {
  completed : Coords.t list;
  start_by_path : bool;
  end_by_path : bool;
}

let orr o o' = match o with None -> o' | _ -> o

let pp_list pp fmt l =
  let rec inner fmt = function
    | [] -> ()
    | x :: l -> Format.fprintf fmt " %a,@; %a " pp x inner l
  in
  Format.fprintf fmt "[@[ %a @]]" inner l

(* TODO: bad junction when end is perpendicular to a single path *)
let paths_layer style { points; cuts; edges; ends; _ } =
  let arity_map =
    CoordMap.empty
    |> (* Adding junctions with their arity *)
    CoordSet.fold
      (fun pos ->
        edges
        |> Edges.filter (Edge.is_adjacent pos)
        |> Edges.cardinal |> CoordMap.add pos)
      (CoordSet.union points cuts)
  in
  (* decrease arity of points adjacent to the edge given *)
  let decrease_arity edge map =
    (* decrease arity of a point *)
    let decrease_arity pos =
      CoordMap.update pos (function
        | None | Some 0 -> None
        | Some i -> Some (i - 1))
    in
    let c, c' = Edge.get edge in
    map |> decrease_arity c |> decrease_arity c'
  in
  let rec synthetize_path complete_path arity_map edges =
    (* Format.printf "Synthesizing %a\n" (pp_list pp_coords) complete_path; *)
    let path = start_path arity_map edges in
    let hole_size = (cell_size -. path_width) /. 3. in
    let rec iter_points ?(is_first = false) acc = function
      | [] -> [ acc ]
      | c :: c' :: l when CoordSet.mem c cuts ->
          (* Format.printf "Found a cut at %a\n" pp_coords c; *)
          let p, p' = (v2 c, v2 c') in
          let dir = V2.(p' - p) in
          acc
          :: iter_points
               [ V2.(p + (hole_size /. 2. * dir / norm dir)) ]
               (c' :: l)
      | c :: c' :: l when CoordSet.mem c' cuts ->
          (* Format.printf "Found a cut at %a\n" pp_coords c'; *)
          let p, p' = (v2 c, v2 c') in
          let dir = V2.(p' - p) in
          (V2.(p' - (hole_size /. 2. * dir / norm dir)) :: p :: acc)
          :: iter_points [] (c' :: l)
      | c :: c' :: l when IntMap.exists (fun _ -> ( = ) c) ends ->
          let p, p' = (v2 c, v2 c') in
          let dir = V2.(p' - p) in
          iter_points [ V2.(p + (cell_size /. 2. * dir / norm dir)) ] (c' :: l)
      | c :: c' :: l when IntMap.exists (fun _ -> ( = ) c') ends ->
          let p, p' = (v2 c, v2 c') in
          let dir = V2.(p' - p) in
          (V2.(p' - (cell_size /. 2. * dir / norm dir)) :: p :: acc)
          :: iter_points [] l
      | c :: c' :: l when is_first ->
          let p, p' = (v2 c, v2 c') in
          let dir = V2.(p' - p) in
          iter_points
            (V2.(p - (path_width /. cell_size * dir / norm dir)) :: acc)
            (c' :: l)
      | [ c; c' ]
      (* c' is not an end nor a cut path because of the patterns above *) ->
          let p, p' = (v2 c, v2 c') in
          let dir = V2.(p' - p) in
          iter_points
            (V2.(p' + (path_width /. cell_size * dir / norm dir)) :: p :: acc)
            []
      | c :: l -> iter_points (v2 c :: acc) l
    in
    complete_path
    |> iter_points ~is_first:true []
    |> List.fold_left
         (fun path -> function
           | s :: l ->
               let path = path |> P.sub s in
               List.fold_left (Fun.flip P.line) path l
           | [] -> path)
         path
  and iter_from_path ({ completed; last_edge; _ } as path) arity_map edges =
    (* Format.printf "Go through edge %a to %a\n" Edge.pp last_edge Coords.pp
       (List.hd completed); *)
    (* try to find the next edge in the same direction as last_edge *)
    (* possible edges to cross *)
    let candidate_edges =
      edges |> Edges.filter (Edge.is_adjacent (List.hd completed))
    in
    (* edge opposite to the last one crossed, if in candidates *)
    let opposite_edge =
      candidate_edges
      |> Edges.filter (fun e -> Edge.aligned e last_edge)
      |> Edges.min_elt_opt
    in
    (* select the opposite edge, or a candidate edge otherwise *)
    let edge = orr opposite_edge (Edges.min_elt_opt candidate_edges) in
    match edge with
    | Some edge ->
        (* Format.printf "Found edge %a to continue\n" Edge.pp edge; *)
        let next_point = Edge.other_end edge (List.hd completed) in
        (if CoordMap.mem next_point arity_map then
           iter_from_path
             { path with completed = next_point :: completed; last_edge = edge }
         else synthetize_path (next_point :: completed))
          (decrease_arity edge arity_map)
          (Edges.remove edge edges)
    | None ->
        (* No edges left to continue the path *)
        synthetize_path completed arity_map edges
  and start_path arity_map edges =
    (* select a path that has only one edge first *)
    match
      arity_map |> CoordMap.filter (fun _ -> ( = ) 1) |> CoordMap.choose_opt
    with
    | Some (pos, _) ->
        (* Format.printf "Start by junction at %a\n" pp_coords pos; *)
        let edge =
          edges |> Edges.filter (Edge.is_adjacent pos) |> Edges.min_elt
        in
        let pos' = Edge.other_end edge pos in
        (* check path exists on the other size *)
        (if CoordMap.mem pos' arity_map then
           iter_from_path
             ({
                start_by_path = true;
                completed = [ pos'; pos ];
                last_edge = edge;
              }
               : incomplete_path)
         else synthetize_path [ pos'; pos ])
          (decrease_arity edge arity_map)
          (Edges.remove edge edges)
    | None -> (
        (* TODO: select a path with more than one edge *)
        (* select an edge otherwise *)
        match edges |> Edges.min_elt_opt with
        | Some edge ->
            (* Format.printf "Start by edge %a\n" pp_edge edge; *)
            (* select a point in a direction to go to. c is the start, c' the current end *)
            let c, c' = Edge.get edge in
            let c, c' =
              if CoordMap.mem c arity_map then (c', c)
                (* choose c as the current point *)
              else (c, c')
            in
            (if CoordMap.mem c arity_map || CoordMap.mem c' arity_map then
               iter_from_path
                 {
                   start_by_path = false;
                   completed = [ c'; c ];
                   last_edge = edge;
                 }
             else
               (* Edge disconnected from both sides *)
               synthetize_path [ c'; c ])
              (decrease_arity edge arity_map)
              (Edges.remove edge edges)
        | None ->
            (* Printf.printf "Starting nowhere, no edge found\n"; *)
            P.empty
            (* No other paths can be created *))
  in
  let path = start_path arity_map edges in
  let area = `O { P.o with width = path_width; join = `Round } in
  I.cut ~area path (I.const style#navigation_color)

let start_layer style starts =
  CoordMap.fold
    (fun (i, j) b ->
      let color = if b then style#navigation_color else style#disabled_color
      and circ = P.circle V2.zero path_width P.empty in
      color |> I.const |> I.cut circ
      |> I.move V2.(v (float_of_int i) (float_of_int j))
      |> I.blend)
    starts I.void

let end_layer style { ends; edges; _ } =
  IntMap.fold
    (fun _ pos ->
      let pos' =
        edges
        |> Edges.filter (Edge.is_adjacent pos)
        |> Edges.choose
        |> Fun.flip Edge.other_end pos
      in
      let di, dj = Coords.(pos' +: ( -: ) pos) in
      let dir = V2.of_tuple (float_of_int di, float_of_int dj) in
      let path =
        V2.
          [ cell_size /. 2. * dir / norm dir; cell_size /. 4. * dir / norm dir ]
        |> path_from_points
      in
      let area = `O { P.o with width = path_width; cap = `Round } in
      I.const style#navigation_color
      |> I.cut ~area path
      |> I.move (v2 pos)
      |> I.blend)
    ends I.void

let symbol_layer style symbols =
  CoordMap.fold
    (fun (i, j) (symbol, color) ->
      symbol
      |> render_symbol style (of_color color)
      |> I.move V2.(v (float_of_int i) (float_of_int j))
      |> I.blend)
    symbols I.void

let solution_layer style sol =
  match List.rev_map v2 sol with
  | start :: _ as sol ->
      (* a solution should always have at least two points *)
      let[@warning "-8"] (p :: p' :: sol) = List.rev sol in
      (* changing the last point to adjust it to the end *)
      let dir = V2.(p' - p) in
      let sol = V2.(p + (cell_size /. 4. * dir / norm dir)) :: p' :: sol in
      let path = sol |> path_from_points in
      let circ = P.empty |> P.circle start path_width in
      let base = I.const style#blue_path_color in
      let area =
        `O { P.o with width = path_width; cap = `Round; join = `Round }
      in
      base |> I.cut ~area path |> I.blend (base |> I.cut circ)
  | [] -> I.void

let debug_layer style debug { cells; _ } =
  if debug then
    let cells_path =
      P.empty
      |> CoordSet.fold
           (fun pos ->
             let p = v2 pos in
             let dx = style#debug_cell_size /. 2. *. cell_size in
             let dv = V2.v dx dx in
             P.rect V2.(Box2.of_pts (p - dv) (p + dv)))
           cells
    in
    I.cut cells_path (I.const style#debug_cell_color)
  else I.void

let render ?(path = "output/") ?(solution = []) ?(debug = false) style puzzle =
  (* Printf.printf "Rendering puzzle %s\n" puzzle.name; *)
  (* taille à revoir *)
  let width, height =
    (float_of_int (puzzle.width + 1), float_of_int (puzzle.height + 1))
  in
  let size = Size2.v width height in
  let view = Box2.v P2.o size in
  let img =
    I.const style#background_color
    |> I.blend (paths_layer style puzzle)
    |> I.blend (start_layer style puzzle.starts)
    |> I.blend (end_layer style puzzle)
    |> I.blend (symbol_layer style puzzle.symbols)
    |> I.blend (solution_layer style solution)
    |> I.blend (debug_layer style debug puzzle)
  in
  img
  |> I.move (V2.v (cell_size /. 2.) 0.)
  |> I.scale (V2.v 1. (-1.))
  |> I.move (V2.v 0. (float_of_int puzzle.height))
  |> render_image path size view

module Style = Style
