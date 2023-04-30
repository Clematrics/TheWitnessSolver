type t = int * int

let compare = compare
let ( +: ) (x, y) (x', y') = (x + x', y + y')
let ( *: ) (x, y) n = (n * x, n * y)
