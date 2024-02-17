open System_def
open Component_def

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
  |Some r -> Gfx.get_resource r
  in
  let ctx = Gfx.get_context (Global.window ()) in
  Texture.image_from_surface ctx bg_surf 0 0 256 256 100 100


let init_wall () =
  ignore (Wall.create "wall_top" 0 0 800 10 black );
  ignore (Wall.create "wall_bottom"  0 590 800 10 black );
  ignore (Wall.create "wall_left" 0 10 10 580 black );
  ignore (Wall.create "wall_right" 790 10 10 580 black )

let init_zone () = 
  ignore (Zone.create_tp_entree "Entree" "Sortie" 100 460 20 20);
  ignore (Zone.create "Sortie" 100 100 20 20 4);
  ignore (Zone.create "Death" 450 450 20 20 2);
  let texture = load_texture_img false true true false in 
  ignore (Zone.create_moov "zone1" 700 10 100 100 texture false true true false);
  let texture = load_texture_img true false false true in 
  ignore (Zone.create_moov "zone2" 0 500 100 100 texture true false false true )

let player = Player.create "player" 50 460 10 10 red
let camera = Camera.create "camera" 0 0 800 600

let has_key, set_key, unset_key =
  let h = Hashtbl.create 16 in
  (fun s-> Hashtbl.mem h s),
  (fun s -> Hashtbl.replace h s ()), 
  (fun s-> Hashtbl.remove h s)



let init dt =
  init_wall ();
  Ecs.System.init_all dt;
  init_zone ();

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