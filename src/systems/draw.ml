open Component_def

(*0 = camera
  1 = player
  2 = wall
  3 = zone*)

type t = drawable

let init _ = ()

let white = Gfx.color 255 255 255 255

let update_aux cam d ctx surf  = 
  let Rect.{width; height} = d # rect # get in
  let Vector.{x; y} =Vector.sub  d # pos # get cam#pos#get in
  let s_pos,r_pos = Rect.mdiff cam#pos#get cam#rect#get d#pos#get d#rect#get in 
  if Rect.has_origin s_pos r_pos then begin 
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
    end

let update _dt el =
  let window = Global.window () in
  let ctx = Gfx.get_context window in
  let surf = Gfx.get_surface window in
  let ww, wh = Gfx.get_context_logical_size ctx in
  Gfx.set_color ctx white;
  Gfx.fill_rect ctx surf 0 0 ww wh;
  let cam = Seq.filter_map (fun (e:t) -> if e#layer#get = 0 then Some e else None) el in
  let walls = Seq.filter_map (fun (e:t) -> if e#layer#get = 2 then Some e else None) el in
  let zones = Seq.filter_map (fun (e:t) -> if e#layer#get = 3 then Some e else None) el in
  let player = Seq.filter_map (fun (e:t) -> if e#layer#get = 1 then Some e else None) el in
  Seq.iter (fun cam ->
    Seq.iter (fun d ->
      update_aux cam d ctx surf
    ) zones;
    Seq.iter (fun d ->
      update_aux cam d ctx surf
    ) player;
    Seq.iter (fun d ->
      update_aux cam d ctx surf
    ) walls
  )cam;
  Gfx.commit ctx