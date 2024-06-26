open System_def
open Component_def

(* On crée une fenêtre *)
let () = Global.init (Format.sprintf "game_canvas:%dx%d:r=presentvsync" 800 600)

let blue = Texture.color (Gfx.color 0 0 255 255)
let black = Texture.color (Gfx.color 0 0 0 255)
let red = Texture.color (Gfx.color 255 0 0 255)
let level = 1

let camera = Camera.create "camera" 0 0 800 600 
let player = Player.create "player" 50 470 10 10 red level

let load_image dst path =
  let ctx = Gfx.get_context(Global.window ()) in
  dst := Some (Gfx.load_image ctx path)

let wait_textures rsc _dt =
  match !rsc with
    None -> failwith "Error"
    | Some r -> not (Gfx.resource_ready r)


(*Zone de test créée "à la main"*)
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
  let texture = Texture_manager.load_texture_img false true true false in 
  ignore (Zone.create_moov "zone1" 700 10 100 100 texture false true true false);
  let texture = Texture_manager.load_texture_img true false false true in 
  ignore (Zone.create_moov "zone2" 0 500 100 100 texture true false false true )


(*Niveau 10 créé "à la main"*)
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

let init_zone () = 
  ignore(Zone.create_death "death_right" 1150 400 50 200);
  ignore(Zone.create_death "death_up" 0 (-200) 400 50);
  ignore(Zone.create_death "death_ver" 210 200 50 200);
  ignore (Zone.create_death "death_bottom_left" 0 550 50 50);
  ignore (Zone.create_moov "bas_droite" 210 0 100 100 (Texture_manager.load_texture_img false true false true) false true false true);
  ignore (Zone.create_moov "bas_gauche" 810 100 100 100 (Texture_manager.load_texture_img false true true false) false true true false);
  ignore (Zone.create_moov "bas_gauche_2" 900 (-390) 100 100 (Texture_manager.load_texture_img false true true false) false true true false);
  ignore (Zone.create "victoire" 650 250 100 100 6 )


let init dt =
  if false then begin 
  init_wall();
  init_zone();
  end 
  else begin
  ignore(Load_lvl.load_lvl level);
  end;
  Ecs.System.init_all dt;
  false

let chain_functions l =
  let todo = ref l in 
  (fun dt -> 
    match !todo with
    [] -> false
    | f :: ll -> 
      let res = f dt in 
      if res then true 
      else begin 
        todo := ll;
        true
      end)

let update dt =
  Ecs.System.update_all dt;
  player#deathed#set (player#deathed#get+1);
  if player#deathed#get = 30 then begin player#moving#set true end;
  if player#won#get then begin
    Load_lvl.load_lvl (player#level#get +1);
    player#level#set (player#level#get +1);
    player#pos#set Vector.{x=50.;y =460.};
    player#haut#set true;
    player#bas#set false;
    player#gauche#set false;
    player#droite#set true;
    camera#pos#set Vector.{x=0.;y=0.};
    player#deathed#set 0;
    player#moving#set false;
    player#won#set false
  end;
  true

let run () =
  Gfx.main_loop (chain_functions 
  [ Texture_manager.load_all_texture;
    Texture_manager.wait_all_textures;
    Level_manager.load_all_level;
    Level_manager.wait_all_level;
    init; 
  update])