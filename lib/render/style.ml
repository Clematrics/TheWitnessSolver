class base_style =
  object
    method blue_path_color = Gg.Color.v_srgbi 0 255 255
    method yellow_path_color = Gg.Color.v_srgbi 255 255 0
    method disabled_color = Gg.Color.v_srgbi 192 192 192
    method hexagon_color = Gg.Color.v_srgbi 0 0 0
    method triangle_color = Gg.Color.v_srgbi 255 128 0
    method shape_color = Gg.Color.v_srgbi 255 255 0
  end

class style =
  object
    inherit base_style
    method background_color = Gg.Color.v_srgbi 255 153 51
    method navigation_color = Gg.Color.v_srgbi 26 13 0
    method path_color = Gg.Color.v_srgbi 0 255 255
    method cell_color = Gg.Color.v_srgbi 0 255 255

    method debug_cell_size = 0.7
    method debug_cell_color = Gg.Color.v_srgbi 255 0 0 ~a:0.1
  end
