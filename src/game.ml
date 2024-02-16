open System_def
open Component_def

(* On crée une fenêtre *)
let () = Global.init (Format.sprintf "game_canvas:%dx%d:r=presentvsync" 800 600)

let blue = Texture.color (Gfx.color 0 0 255 255)
let black = Texture.color (Gfx.color 0 0 0 255)
let red = Texture.color (Gfx.color 255 0 0 255)

let bg_ressource = ref None


let load_bg dst path =
  let ctx = Gfx.get_context(Global.window ()) in
  dst := Some (Gfx.load_image ctx path)

let wait_textures rsc _dt =
  match !rsc with
    None -> failwith "Error"
    | Some r -> not (Gfx.resource_ready r)

let init_wall () =
  ignore (Wall.create "wall_top" 0 0 800 10 black );
  ignore (Wall.create "wall_bottom"  0 590 800 10 black );
  ignore (Wall.create "wall_left" 0 10 10 580 black );
  ignore (Wall.create "wall_right" 790 10 10 580 black )

let init_zone () = 
  
  load_bg  bg_ressource "resources/images/bottom_left.png";
  Gfx.main_loop (wait_textures bg_ressource) ;
  let bg_surf = match !bg_ressource with 
  None -> assert false
  |Some r -> Gfx.get_resource r
  in
  let ctx = Gfx.get_context (Global.window ()) in
  let texture = Texture.anim_from_surface ctx bg_surf 1 256 256 300 300 10000 in
  ignore (Zone.create_moov "zone1" 500 10 300 300 texture false true true false);
  ignore (Zone.create_moov "zone2" 0 500 100 100 (Texture.color (Gfx.color 0 0 0 128)) true false false true );
  ignore (Zone.create_tp_entree "Entree" "Sortie" 100 460 20 20);
  ignore (Zone.create "Sortie" 100 100 20 20 4);
  ignore (Zone.create "Death" 450 450 20 20 5)

let player = Player.create "player" 50 460 10 10 red

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