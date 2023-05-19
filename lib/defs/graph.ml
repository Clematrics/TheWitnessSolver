type t = { edges : Edges.t; points : CoordSet.t }

let empty = { edges = Edges.empty; points = CoordSet.empty }
let edges g = g.edges
let points g = g.points
let add_edge e g = { g with edges = Edges.add e g.edges }

let add_edges edges g =
  { g with edges = Edges.add_seq (List.to_seq edges) g.edges }

let add_point pos g = { g with points = CoordSet.add pos g.points }
let adjacent_edges pos g = g.edges |> Edges.filter (Edge.is_adjacent pos)

let remove pos g =
  {
    edges = Edges.diff g.edges (adjacent_edges pos g);
    points = CoordSet.remove pos g.points;
  }

let fold_points f init g = CoordSet.fold f init g.points
