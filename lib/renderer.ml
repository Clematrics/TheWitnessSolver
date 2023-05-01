open Gg
open Vg
open Definition
open Puzzle

let background_color = Color.v_srgbi 255 153 51
let navigation_color = Color.v_srgbi 26 13 0
let blue_path_color = Color.v_srgbi 0 255 255
let yellow_path_color = Color.v_srgbi 255 255 0
let disabled_color = Color.v_srgbi 192 192 192
let triangle_color = Color.v_srgbi 255 128 0

let of_color = function
  | AnyColor -> Color.v_srgbi 42 42 42
  | Red -> Color.v_srgbi 255 0 0
  | Green -> Color.v_srgbi 0 255 0
  | Blue -> Color.v_srgbi 0 0 255
  | White -> Color.v_srgbi 255 255 255
  | Cyan -> Color.v_srgbi 0 255 255
  | Magenta -> Color.v_srgbi 255 0 255
  | Yellow -> Color.v_srgbi 255 255 0
  | Black -> Color.v_srgbi 0 0 0
  | Orange -> Color.v_srgbi 255 128 0

let path_width = 0.5
let cell_size = 2.
let cell_inner = cell_size -. path_width

let path_from_points ?(rel = false) = function
  | [] -> P.empty
  | p :: l -> List.fold_left (Fun.flip (P.line ~rel)) (P.sub ~rel p P.empty) l

let shape_from_points ?(rel = false) p = p |> path_from_points ~rel |> P.close
let points_from_coords = List.map V2.of_tuple

(* Peut-être factorisé *)
let img_from_path elt =
  let rec draw = function
    | Meet ->
        let img =
          P.empty
          |> P.rect
               (Box2.v
                  V2.(zero - (v path_width path_width / 2.))
                  V2.(v path_width path_width))
          |> Fun.flip I.cut (I.const navigation_color)
        in
        if OffsetSet.cardinal elt.connected_paths = 2 then
          let x, y =
            OffsetSet.fold
              (fun o -> o |> Offset.map_coords |> Coords.( +: ))
              elt.connected_paths (0, 0)
          in
          let c =
            V2.( * ) (path_width /. 2.) (V2.v (float_of_int x) (float_of_int y))
          in
          I.cut (P.empty |> P.circle c path_width) img
        else img
    | Start b ->
        let circ = P.circle V2.zero path_width P.empty in
        I.cut circ (I.const (if b then navigation_color else disabled_color))
    | End _ ->
        let dx, dy =
          OffsetSet.choose elt.connected_paths |> Offset.map_coords
        in
        let path =
          [
            ( cell_size /. 2. *. float_of_int dx,
              cell_size /. 2. *. float_of_int dy );
            (0., 0.);
          ]
          |> points_from_coords |> path_from_points
        in
        let area = `O { P.o with width = path_width; cap = `Round } in
        I.cut ~area path (I.const navigation_color)
    | PathHorizontal b ->
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
    | PathVertical b -> draw (PathHorizontal b) |> I.rot (-.Float.pi /. 2.)
  in
  elt.path |> Option.map draw |> Option.value ~default:I.void

let img_from_shape color shape r =
  let bsize = 0.9 in
  let block (i, j) =
    P.rect
      (Box2.v (V2.v (float_of_int i) (float_of_int j)) (V2.v bsize bsize))
      P.empty
  in
  let merge = List.fold_left P.append P.empty in
  let coords = function
    | Block -> [ (0, 0) ]
    | Block4 -> [ (0, 0); (0, 1); (1, 0); (1, 1) ]
    | Bar2 -> [ (0, 0); (1, 0) ]
    | Bar3 -> [ (0, 0); (1, 0); (2, 0) ]
    | Bar4 -> [ (0, 0); (1, 0); (2, 0); (3, 0) ]
    | Corner -> [ (0, 0); (1, 0); (0, 1) ]
    | L -> [ (0, 0); (1, 0); (2, 0); (0, 1) ]
    | LMirrored -> [ (0, 0); (1, 0); (0, 1); (0, 2) ]
    | T -> [ (0, 0); (1, 0); (2, 0); (1, 1); (1, 2) ]
    | SmallT -> [ (0, 0); (1, 0); (2, 0); (1, 1) ]
    | Diagonal2 -> [ (0, 0); (1, 1) ]
  in
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
  let rotation = function
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
  let coords = coords shape in
  let box = box coords in
  let img =
    coords |> List.map block |> merge |> Fun.flip I.cut (I.const color)
  in
  frame box (rotation r) img

let img_from_triangle i =
  let t =
    [ (0., 0.); (1., 0.); (0.5, -.sqrt 3. /. 2.) ]
    |> points_from_coords |> shape_from_points
  in
  let triangle = I.cut t (I.const triangle_color) in
  let triangle2 = I.move (V2.v 1.2 0.) triangle in
  let triangle3 = I.move (V2.v 0.5 1.2) triangle in
  let img =
    match i with
    | 1 -> triangle
    | 2 -> I.blend triangle triangle2
    | 3 -> I.blend triangle (I.blend triangle2 triangle3)
    | _ -> assert false
  in
  I.scale (V2.v (1. /. 3.) (1. /. 3.)) img

let img_from_symbol color =
  let on_circle r theta = V2.of_polar (V2.v r theta) in
  let angles cnt =
    List.init cnt (fun i ->
        float_of_int i *. 2. *. Float.pi /. float_of_int cnt)
  in
  function
  | Hexagon ->
      let angles = angles 6 in
      let points = List.map (on_circle 0.1) angles in
      let path = shape_from_points points in
      I.cut path (I.const color)
  | Square ->
      let box_size = 7. /. 5. *. path_width in
      let round = path_width /. 5. in
      let pos = 4. /. 5. *. path_width in
      let rect =
        P.rrect
          (Box2.v V2.zero (V2.v box_size box_size))
          (V2.v round round) P.empty
      in
      I.cut rect (I.const color) |> I.move (V2.v pos pos)
  | Star ->
      let path =
        angles 8
        |> List.map (on_circle (path_width /. 2.))
        |> List.mapi (fun i p -> if i mod 2 = 0 then V2.(7. /. 5. * p) else p)
        |> shape_from_points
      in
      I.cut path (I.const color)
  | Shape (s, r) -> img_from_shape color s r
  | AntiShape (s, r) -> img_from_shape color s r (* TODO: *)
  | Triangle i -> img_from_triangle i
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
      I.move (V2.v 1. 1.) img

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

let render ?(prefix_path="output/") puzzle =
  let path = Printf.sprintf "%s%s.svg" prefix_path puzzle.name in
  (* taille à revoir *)
  let width, height =
    (float_of_int (puzzle.width + 1), float_of_int (puzzle.height + 1))
  in
  let size = Size2.v width height in
  let view = Box2.v P2.o size in
  let img =
    Board.fold
      (fun (x, y) elt img ->
        (* match elt.path with
           | Some Meet -> *)
        let img_nav = img_from_path elt
        and img_sym =
          elt.symbol
          |> Option.map (fun (s, c) -> img_from_symbol (of_color c) s)
          |> Option.value ~default:I.void
        in
        I.blend img_sym img_nav
        |> I.move (V2.v (float_of_int x) (float_of_int y))
        |> Fun.flip I.blend img
        (* | _ -> img *))
      puzzle.board (I.const background_color)
  in
  img
  |> I.move (V2.v (cell_size /. 2.) 0.)
  |> I.scale (V2.v 1. (-1.))
  |> I.move (V2.v 0. (float_of_int puzzle.height))
  |> render_image path size view
