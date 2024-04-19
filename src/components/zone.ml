open Component_def
open System_def

let trans_black = Texture.color (Gfx.color 0 0 0 128)
let trans_red = Texture.color (Gfx.color 255 0 0 128)
let trans_blue = Texture.color (Gfx.color 0 0 255 128)
let trans_light_blue = Texture.color (Gfx.color 128 128 255 128)
let trans_yellow = Texture.color (Gfx.color 255 255 0 128)
let black = Texture.color (Gfx.color 0 0 0 255)



(*1 = moov
  2 = death zone
  3 = téléportation entrée ?
  4 = tp sortie
  5 = ice
  6 = victoire*)
let create id x y w h effect =
  (**
      [create id x y w h effect]*)
  let z = new zone in 
  z # pos # set Vector.{x = float x; y = float y};
  z # rect # set Rect.{width = w; height = h};
  let () = match effect with
  2 -> z#texture#set trans_black
  |3 ->z#texture#set trans_red
  |4 ->z#texture#set trans_blue 
  |5 ->z#texture#set trans_light_blue
  |6 ->z#texture#set trans_yellow
  |_ ->z#texture#set trans_blue
in 
  z # id # set id;
  z # layer # set 3;
  z #effect # set effect;
  Draw_system.register ( z:> drawable);
  Zonable_System.register (z :> zonable);
  Next_lvl_syst.register (z :>cancellable);
  z #remove#set (fun () -> 
    Zonable_System.unregister (z :> zonable);
    Draw_system.unregister (z :> drawable));
    Next_lvl_syst.register (z :>cancellable);
  z

let create_moov id x y w h texture haut bas gauche droite =
  (**
      [create_moov id x y w h texture haut bas gauche droite]*)
  let z = create id x y w h 1 in 
  z # texture #set texture;
  z # haut # set haut;
  z # bas # set bas;
  z # gauche # set gauche;
  z # droite # set droite;
  z

let create_death id x y w h =
  let z = create id x y w h 2 in 
  z # haut # set true;
  z # bas # set false;
  z # gauche # set false;
  z # droite # set true;
  z

let create_tp_entree id sibling x y w h =
  let z = create id x y w h 3 in 
  z # sibling # set sibling;
  z

let create_victory id x y w h =
  let z = create id x y w h 6 in 
  z # pos # set Vector.{x = float x +. 50.; y = float y +. 50.};
  z