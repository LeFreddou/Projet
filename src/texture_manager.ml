open Component_def
open System_def

let texture_table = Hashtbl.create 32

let paths = ["resources/images/all_moov.png";
             "resources/images/bottom_up_left.png";
             "resources/images/bottom_up_right.png";
             "resources/images/bottom_up.png";
             "resources/images/up_left_right.png";
             "resources/images/up_left.png";
             "resources/images/up_right.png";
             "resources/images/up.png";
             "resources/images/bottom_left_right.png";
             "resources/images/bottom_left.png";
             "resources/images/bottom_right.png";
             "resources/images/bottom.png";
             "resources/images/right_left.png";
             "resources/images/left.png";
             "resources/images/right.png";
             "resources/images/placeholder.png"]



let load_all_texture _dt =
  let ctx = Gfx.get_context(Global.window ()) in
  List.iter (fun path -> Hashtbl.add texture_table path (Gfx.load_image ctx path)) paths;
  false


let wait_all_textures _dt = 
  let all_loaded = Hashtbl.fold (fun _p txt acc -> Gfx.resource_ready txt && acc) texture_table true in 
  not (all_loaded)


let load_texture_img haut bas gauche droite =
  (**
    [load_texture_img haut bas gauche droite]*)
  let path =
  match haut,bas,gauche,droite with
  true , true, true, true -> "resources/images/all_moov.png"
  |true, true, true,false -> "resources/images/bottom_up_left.png"
  |true, true, false, true-> "resources/images/bottom_up_right.png"
  |true, true, false, false-> "resources/images/bottom_up.png"
  |true, false, true, true-> "resources/images/up_left_right.png"
  |true, false, true, false-> "resources/images/up_left.png"
  |true, false, false, true-> "resources/images/up_right.png"
  |true, false, false, false-> "resources/images/up.png"
  |false, true, true, true-> "resources/images/bottom_left_right.png"
  |false ,true,true,false -> "resources/images/bottom_left.png"
  |false, true, false, true-> "resources/images/bottom_right.png"
  |false, true, false, false-> "resources/images/bottom.png"
  |false,false,true,true -> "resources/images/right_left.png"
  |false, false, true, false-> "resources/images/left.png"
  |false, false, false, true-> "resources/images/right.png"
  |_ -> "resources/images/placeholder.png"
  in
  let ressource = Hashtbl.find texture_table path in 
  let bg_surf = try Gfx.get_resource ressource with e -> let msg = Printexc.to_string e in Gfx.debug "%s\n%!" msg; failwith msg
  in
  let ctx = Gfx.get_context (Global.window ()) in
  Texture.image_from_surface ctx bg_surf 0 0 256 256 100 100