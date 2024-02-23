open System_def
open Component_def
open Load_lvl

(* On crée une fenêtre *)
let () = Global.init (Format.sprintf "game_canvas:%dx%d:r=presentvsync" 800 600)

let blue = Texture.color (Gfx.color 0 0 255 255)
let black = Texture.color (Gfx.color 0 0 0 255)
let red = Texture.color (Gfx.color 255 0 0 255)

let ressource = ref None


let load_image dst path =
  let ctx = Gfx.get_context(Global.window ()) in
  dst := Some (Gfx.load_image ctx path)

let wait_textures rsc _dt =
  match !rsc with
    None -> failwith "Error"
    | Some r -> not (Gfx.resource_ready r)

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
  load_image ressource path;
  Gfx.main_loop (wait_textures ressource) ;
  let bg_surf = match !ressource with 
  None -> assert false
  |Some r ->try Gfx.get_resource r with e -> let msg = Printexc.to_string e in Gfx.debug "%s\n%!" msg; failwith msg
  in
  let ctx = Gfx.get_context (Global.window ()) in
  Texture.image_from_surface ctx bg_surf 0 0 256 256 100 100


let init_wall_test () =
  ignore (Wall.create "wall_top" 0 0 800 10 black );
  ignore (Wall.create "wall_bottom"  0 590 800 10 black );
  ignore (Wall.create "wall_left" 0 10 10 580 black );
  ignore (Wall.create "wall_right" 790 10 10 580 black )

let init_zone_test () = 
  ignore (Zone.create_tp_entree "Entree" "Sortie" 100 460 20 20);
  ignore (Zone.create "Sortie" 100 100 20 20 4);
  ignore (Zone.create "Death" 450 450 20 20 2);
  ignore (Zone.create "Glace" 300 300 200 100 5);
  let texture = load_texture_img false true true false in 
  ignore (Zone.create_moov "zone1" 700 10 100 100 texture false true true false);
  let texture = load_texture_img true false false true in 
  ignore (Zone.create_moov "zone2" 0 500 100 100 texture true false false true )


let init_wall () =
  ignore (Wall.create "left_wall" 0 (-200) 10 800 black);
  ignore (Wall.create "bottom_wall" 0 590 1200 10 black);
  ignore (Wall.create "mur_hor_1" 200 400 610 10 black);
  ignore (Wall.create "mur_ver_1" 200 0 10 400 black);
  ignore (Wall.create "up_wall_1" 0 (-200) 400 10 black);
  ignore (Wall.create "mur_hor_2" 1000 400 200 10 black);
  ignore (Wall.create "right_wall_1" 1200 400 10 200 black);
  ignore (Wall.create "mur_ver_2" 800 200 10 200 black);
  ignore (Wall.create "mur_ver_3" 800 (-400) 10 400 black);
  ignore (Wall.create "up_wall_3" 800 (-400) 200 10 black);
  ignore (Wall.create "right_wall_2" 1000 (-400) 10 800 black);
  ignore (Wall.create "mur_haut_2" 400 0 410 10 black);
  ignore (Wall.create "mur_ver_4" 400 (-200) 10 200 black);
  ignore (Wall.create "mur_hor_3" 400 200 410 10 black)





(*1 = moov
  2 = death zone
  3 = téléportation entrée ?
  4 = tp sortie*)
let init_zone () = 
  ignore(Zone.create_death "death_right" 1150 400 50 200);
  ignore(Zone.create_death "death_up" 0 (-200) 400 50);
  ignore(Zone.create_death "death_ver" 210 200 50 200);
  ignore (Zone.create_death "death_bottom_left" 0 550 50 50);
  ignore (Zone.create_moov "bas_droite" 210 0 100 100 (load_texture_img false true false true) false true false true);
  ignore (Zone.create_moov "bas_gauche" 810 100 100 100 (load_texture_img false true true false) false true true false);
  ignore (Zone.create_moov "bas_gauche_2" 900 (-390) 100 100 (load_texture_img false true true false) false true true false);
  ignore (Zone.create "victoire" 650 250 100 100 5 )


let player = Player.create "player" 50 460 10 10 red
let camera = Camera.create "camera" 0 0 800 600

let has_key, set_key, unset_key =
  let h = Hashtbl.create 16 in
  (fun s-> Hashtbl.mem h s),
  (fun s -> Hashtbl.replace h s ()), 
  (fun s-> Hashtbl.remove h s)



let init dt =
  init_wall_test();
  init_zone_test();
  Ecs.System.init_all dt;
  false


let has_key, set_key, unset_key =
  let h = Hashtbl.create 16 in
  (fun s-> Hashtbl.mem h s),
  (fun s -> Hashtbl.replace h s ()), 
  (fun s-> Hashtbl.remove h s)


let update dt =
  Ecs.System.update_all dt;
  not (has_key "Enter")

let run () =
  Gfx.main_loop init;
  Gfx.main_loop update