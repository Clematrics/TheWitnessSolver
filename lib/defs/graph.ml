type t = { edges : Edges.t; points : CoordSet.t }

let empty = { edges = Edges.empty; points = CoordSet.empty }
let edges g = g.edges
let points g = g.points

let add_edge e g =
  let p, p' = Edge.get e in
  let points = CoordSet.(g.points |> add p |> add p') in
  let edges = Edges.add e g.edges in
  { points; edges }

let add_edges edges g =
  let points =
    List.fold_left
      CoordSet.(
        fun points e ->
          let p, p' = Edge.get e in
          points |> add p |> add p')
      g.points edges
  in
  let edges = List.fold_left (Fun.flip Edges.add) g.edges edges in
  { points; edges }

let add_point pos g = { g with points = CoordSet.add pos g.points }
let mem g p = CoordSet.mem p g.points
let adjacent_edges pos g = g.edges |> Edges.filter (Edge.is_adjacent pos)

let remove pos g =
  {
    edges = Edges.diff g.edges (adjacent_edges pos g);
    points = CoordSet.remove pos g.points;
  }

let edge_through p g =
  g.edges |> Edges.filter (fun e -> Edge.pass_through e p) |> Edges.choose_opt

let arity p g = Edges.cardinal (adjacent_edges p g)

let arity_map g =
  let update_map = function None -> Some 1 | Some i -> Some (i + 1) in
  CoordMap.empty
  |> Edges.fold
       (fun edge map ->
         let p, p' = Edge.get edge in
         map |> CoordMap.update p update_map |> CoordMap.update p' update_map)
       g.edges

let remove_edge e g =
  let p, p' = Edge.get e in
  let update_point p points =
    if arity p g = 1 then CoordSet.remove p points else points
  in
  let edges = Edges.remove e g.edges in
  let points = g.points |> update_point p |> update_point p' in
  { points; edges }

let fold_points f g init = CoordSet.fold f g.points init
let fold_edges f g init = Edges.fold f g.edges init

let union { points; edges } g' =
  {
    points = CoordSet.union points g'.points;
    edges = Edges.union edges g'.edges;
  }

let pp fmt { points; edges } =
  Format.fprintf fmt "@[<hov 4>{@;points = %a;@;edges = %a@;}@]"
    (CoordSet.pp Coord.pp) points (Edges.pp Edge.pp) edges
