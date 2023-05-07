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

let path_from_points ?(rel = false) = function
  | [] -> P.empty
  | p :: l -> List.fold_left (Fun.flip (P.line ~rel)) (P.sub ~rel p P.empty) l

let shape_from_points ?(rel = false) p = p |> path_from_points ~rel |> P.close
let points_from_coords = List.map V2.of_tuple

(* Peut-être factorisé *)
let render_path style board pos elt = function
  | Path _ ->
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
      List.fold_left
        (fun img o ->
          match Board.find_opt Coords.(pos +: o) board with
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
          (0., 0.);
        ]
        |> points_from_coords |> path_from_points
      in
      let area = `O { P.o with width = path_width; cap = `Round } in
      I.cut ~area path (I.const style#navigation_color)
(* | PathHorizontal b ->
       let path =
         let e1, e2 =
           ((path_width -. cell_size) /. 2., (cell_size -. path_width) /. 2.)
         in
         if b then
           [ (e1, 0.); (e2, 0.) ] |> points_from_coords |> path_from_points
         else
           let path =
             [ (e1, 0.); (-.path_width /. 2., 0.) ]
             |> points_from_coords |> path_from_points
           and path' =
             [ (path_width /. 2., 0.); (e2, 0.) ]
             |> points_from_coords |> path_from_points
           in
           P.append path' path
       in
       let area = `O { P.o with width = path_width } in
       I.cut ~area path (I.const navigation_color)
   | PathVertical b ->
       render_path board pos elt (PathHorizontal b) |> I.rot (-.Float.pi /. 2.) *)

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
    let size = max (Box2.w box) (Box2.h box) in
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

(* let img_nav = img_from_path elt
   and img_sym =
     elt.symbol
     |> Option.map (fun (s, c) -> render_symbol (of_color c) s)
     |> Option.value ~default:I.void
   in *)
let place_on background (x, y) img =
  img
  |> I.move (V2.v (float_of_int x) (float_of_int y))
  |> Fun.flip I.blend background

let create_paths_layer style board =
  Board.fold
    (fun pos elt img ->
      match elt.path with
      | None | Some (Start _) -> img
      | Some p -> render_path style board pos elt p |> place_on img pos)
    board

let create_start_layer style board =
  Board.fold
    (fun pos elt img ->
      match elt.path with
      | Some (Start _ as p) ->
          render_path style board pos elt p |> place_on img pos
      | _ -> img)
    board

let create_symbol_layer style =
  Board.fold (fun pos elt img ->
      elt.symbol
      |> Option.map (fun (s, c) -> render_symbol style (of_color c) s)
      |> Option.value ~default:I.void
      |> place_on img pos)

let render ?(prefix_path = "output/") style puzzle =
  let path = Printf.sprintf "%s%s.svg" prefix_path puzzle.name in
  (* taille à revoir *)
  let width, height =
    (float_of_int (puzzle.width + 1), float_of_int (puzzle.height + 1))
  in
  let size = Size2.v width height in
  let view = Box2.v P2.o size in
  let background_layer = I.const style#background_color
  and paths_layer = I.void |> create_paths_layer style puzzle.board
  and start_layer = I.void |> create_start_layer style puzzle.board
  and symbol_layer = I.void |> create_symbol_layer style puzzle.board in
  let img =
    List.fold_left I.blend I.void
      [ symbol_layer; start_layer; paths_layer; background_layer ]
  in
  img
  |> I.move (V2.v (cell_size /. 2.) 0.)
  |> I.scale (V2.v 1. (-1.))
  |> I.move (V2.v 0. (float_of_int puzzle.height))
  |> render_image path size view

module Style = Style