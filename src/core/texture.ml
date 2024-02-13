type t = Color of Gfx.color
|Image of Gfx.surface

let color c = Color c

let image_from_surface ctx surface x y w h dw dh =
  let dst = Gfx.create_surface ctx dw dh in
  Gfx.blit_full ctx dst surface x y 0 0 w h dw dh;
  Image dst