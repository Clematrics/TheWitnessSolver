include Set.Make (Property)

let symmetry_properties =
  empty
  |> add AxialSymmetry
  |> add HorizontalSymmetry
  |> add VerticalSymmetry