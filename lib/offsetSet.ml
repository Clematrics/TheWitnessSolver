include Set.Make (Offset)

open Offset

let make_set = List.fold_left (Fun.flip add) empty
let all = make_set [ SxPy; SxNy; PxPy; NxNy; PxSy; NxSy; NxPy; PxNy ]
let adjacent = make_set [ SxPy; SxNy; PxSy; NxSy ]
let horizontal = make_set [ PxSy; NxSy ]
let vertical = make_set [ SxPy; SxNy ]
let corners = make_set [ PxPy; NxNy; NxPy; PxNy ]
