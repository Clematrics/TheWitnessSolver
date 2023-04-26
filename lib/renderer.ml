open Gg
open Vg
open Definition
open Puzzle

let background_color = Color.v_srgbi 255 153 51
let navigation_color = Color.v_srgbi 26 13 0
let blue_path_color = Color.v_srgbi 0 255 255
let yellow_path_color = Color.v_srgbi 255 255 0

let of_color = function
  | Any -> Color.v_srgbi 42 42 42
  | Red -> Color.v_srgbi 255 0 0
  | Green -> Color.v_srgbi 0 255 0
  | Blue -> Color.v_srgbi 0 0 255
  | White -> Color.v_srgbi 255 255 255
  | Cyan -> Color.v_srgbi 0 255 255
  | Magenta -> Color.v_srgbi 255 0 255
  | Yellow -> Color.v_srgbi 255 255 0
  | Black -> Color.v_srgbi 0 0 0

let path_width = 0.25
let cell_inner = 0.75

let shape_from_points ?(rel = false) = function
  | [] -> P.empty
  | p :: l ->
      List.fold_left (Fun.flip (P.line ~rel)) (P.sub ~rel p P.empty) l
      |> P.close

let path_from_points ?(rel = false) = function
  | [] -> P.empty
  | p :: l -> List.fold_left (Fun.flip (P.line ~rel)) (P.sub ~rel p P.empty) l

let points_from_coords = List.map V2.of_tuple

(* Peut-être factorisé *)
let rec img_from_path = function
  | EmptyNav -> I.const Color.void
  | Meet ->
      let points =
        points_from_coords [ (0., 0.); (1., 0.); (1., 1.); (0., 1.) ]
      in
      let shape = shape_from_points points in
      I.cut shape (I.const navigation_color)
      |> I.scale V2.(path_width * (ox + oy))
  | Start ->
      let circ = P.circle V2.zero path_width P.empty in
      I.cut circ (I.const navigation_color)
  | End _ ->
      (* TODO: *)
      let path =
        [ (0., 0.); (path_width, 0.) ] |> points_from_coords |> path_from_points
      in
      let area = `O { P.o with width = path_width; cap = `Round } in
      I.cut ~area path (I.const navigation_color)
  | PathHorizontal ->
      let path =
        [ (0., 0.); (cell_inner, 0.) ] |> points_from_coords |> path_from_points
      in
      let area = `O { P.o with width = path_width } in
      I.cut ~area path (I.const navigation_color)
  | PathVertical -> img_from_path PathHorizontal |> I.rot (-.Float.pi /. 4.)
  | CutPathHorizontal ->
      let path =
        [ (0., 0.); (path_width, 0.) ] |> points_from_coords |> path_from_points
      and path' =
        [ (2. *. path_width, 0.); (3. *. path_width, 0.) ]
        |> points_from_coords |> path_from_points
      in
      let path = P.append path' path in
      let area = `O { P.o with width = path_width } in
      I.cut ~area path (I.const navigation_color)
      (* let points =
           points_from_coords [ (0., 0.); (1., 0.); (1., 0.); (0., 1.) ]
         in
         let shape = shape_from_points points in
         let bit = I.cut shape (I.const navigation_color) in
         let bit' = I.move (V2.v 3. 0.) bit in
         I.blend bit bit' |> I.scale V2.(path_width * (ox + oy)) *)
  | CutPathVertical ->
      img_from_path CutPathHorizontal |> I.rot (-.Float.pi /. 4.)
(* let points =
     points_from_coords [ (0., 0.); (1., 0.); (1., 0.); (0., 1.) ]
   in
   let shape = shape_from_points points in
   let bit = I.cut shape (I.const navigation_color) in
   let bit' = I.move (V2.v 0. 3.) bit in
   I.blend bit bit' |> I.scale V2.(path_width * (ox + oy)) *)

let img_from_shape color shape =
  let bsize = 0.9 in
  let block (i, j) =
    P.rect
      (Box2.v (V2.v (float_of_int i) (float_of_int j)) (V2.v bsize bsize))
      P.empty
  in
  let merge = List.fold_left P.append P.empty in
  let coords = function
    | Unit -> [ (0, 0) ]
    | TwoBarHorizontal | TwoBarVertical | TwoBarAny -> [ (0, 0); (1, 0) ]
    | ThreeBarHorizontal | ThreeBarVertical | ThreeBarAny ->
        [ (0, 0); (1, 0); (2, 0) ]
    | FourBarHorizontal | FourBarVertical | FourBarAny ->
        [ (0, 0); (1, 0); (2, 0); (3, 0) ]
    | CornerTL | CornerTR | CornerBL | CornerBR | CornerAny ->
        [ (0, 0); (1, 0); (0, 1) ]
    | LTL | LTR | LBL | LBR | LAny -> [ (0, 0); (1, 0); (2, 0); (0, 1) ]
    | LReversedTL | LReversedTR | LReversedBL | LReversedBR | LReversedAny ->
        [ (0, 0); (1, 0); (0, 1); (0, 2) ]
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
  let r90 = Float.pi /. 2.
  and r180 = Float.pi
  and r270 = -.Float.pi /. 2.
  and r_any = Float.pi /. 6. in
  let rotation = function
    | Unit -> 0.
    | TwoBarHorizontal | ThreeBarHorizontal | FourBarHorizontal | CornerTL | LTL
    | LReversedTL ->
        0.
    | TwoBarVertical | ThreeBarVertical | FourBarVertical | CornerTR | LTR
    | LReversedTR ->
        r270
    | CornerBL | LBL | LReversedBL -> r90
    | CornerBR | LBR | LReversedBR -> r180
    | TwoBarAny | ThreeBarAny | FourBarAny | CornerAny | LAny | LReversedAny ->
        r_any
  in
  let frame box rotation img =
    let size = max (Box2.w box) (Box2.h box) in
    I.move V2.(zero - Box2.mid box) img
    |> I.rot rotation
    |> I.scale V2.(1. /. size *. cell_inner *. 0.8 * (ox + oy))
  in
  let coords = coords shape in
  let box = box coords in
  let img =
    coords |> List.map block |> merge |> Fun.flip I.cut (I.const color)
  in
  frame box (rotation shape) img

let img_from_symbol color =
  let on_circle r theta = V2.of_polar (V2.v r theta) in
  let angles cnt =
    List.init cnt (fun i ->
        float_of_int i *. 2. *. Float.pi /. float_of_int cnt)
  in
  function
  | EmptySymbol -> I.const Color.void
  | Hexagon ->
      let angles = angles 6 in
      let points = List.map (on_circle 0.1) angles in
      let path = shape_from_points points in
      I.cut path (I.const color)
      (* TODO: points on a circle of diameter 0.2 *)
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
        |> List.mapi (fun i p -> if i mod 2 == 0 then V2.(7. /. 5. * p) else p)
        |> shape_from_points
      in
      I.cut path (I.const color)
  | Shape s -> img_from_shape color s
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
      I.cut ~area path (I.const color)

let render puzzle =
  (* taille à revoir *)
  let width, height = (float_of_int puzzle.width, float_of_int puzzle.height) in
  let size = Size2.v width height in
  let view = Box2.v P2.o size in
  () (* return size, view, img *)

let render_image size view img =
  try
    let oc = open_out "output.svg" in
    let r = Vgr.create (Vgr_svg.target ()) (`Channel oc) in
    try
      ignore (Vgr.render r (`Image (size, view, img)));
      ignore (Vgr.render r `End);
      close_out oc
    with e ->
      close_out oc;
      raise e
  with Sys_error e -> prerr_endline e
