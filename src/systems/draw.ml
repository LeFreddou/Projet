open Component_def


type t = drawable

let init _ = ()

let white = Gfx.color 255 255 255 255

let update _dt el =
  let window = Global.window () in
  let ctx = Gfx.get_context window in
  let surf = Gfx.get_surface window in
  let ww, wh = Gfx.get_context_logical_size ctx in
  Gfx.set_color ctx white;
  Gfx.fill_rect ctx surf 0 0 ww wh;
  Seq.iter (fun d ->
      
      let Rect.{width; height} = d # rect # get in
      let Vector.{x; y} = d # pos # get in
      let x = int_of_float x in
      let y = int_of_float y in
      match d#texture#get with 
      Texture.Color color ->
      Gfx.set_color ctx color;
      Gfx.fill_rect ctx surf x y width height
      |Texture.Image surface ->
        Gfx.blit_scale ctx surf surface x y width height 
      | Texture.Animation r -> 
          r.current_time <- r.current_time -1;
          if r.current_time = 0 then begin
            r.current_time <- r.frame_duration;
            r.current_frame <- (r.current_frame +1) mod (Array.length r.frames)
          end;
          let surface = r.frames.(r.current_frame) in
          Gfx.blit_scale ctx surf surface x y width height
    ) el;
  Gfx.commit ctx