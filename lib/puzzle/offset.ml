type t = SxPy | SxNy | PxPy | NxNy | PxSy | NxSy | NxPy | PxNy

let map_int = function
  | SxPy -> 0
  | SxNy -> 1
  | PxPy -> 2
  | NxNy -> 3
  | PxSy -> 4
  | NxSy -> 5
  | NxPy -> 6
  | PxNy -> 7

let map_coords = function
  | SxPy -> (0, -1)
  | SxNy -> (0, 1)
  | PxPy -> (-1, -1)
  | NxNy -> (1, 1)
  | PxSy -> (-1, 0)
  | NxSy -> (1, 0)
  | NxPy -> (1, -1)
  | PxNy -> (-1, 1)

let opposite = function
  | SxPy -> SxNy
  | SxNy -> SxPy
  | PxPy -> NxNy
  | NxNy -> PxPy
  | PxSy -> NxSy
  | NxSy -> PxSy
  | NxPy -> PxNy
  | PxNy -> NxPy

let compare o o' = compare (map_int o) (map_int o')
