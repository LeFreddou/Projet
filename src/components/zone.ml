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
  5 = ice*)
let create id x y w h effect =
  (**
      [create id x y w h effect]*)
  let z = new zone in 
  z # pos # set Vector.{x = float x; y = float y};
  z # rect # set Rect.{width = w; height = h};
  let () = match effect with
  4 ->z#texture#set trans_blue 
  |5 -> z#texture#set trans_light_blue
  |_ -> z # texture # set trans_yellow
in 
  z # id # set id;
  z # layer # set 3;
  z #effect # set effect;
  Draw_system.register ( z:> drawable);
  Zonable_System.register (z :> zonable);
  z


let create_moov id x y w h texture haut bas gauche droite =
  (**
      [create_moov id x y w h texture haut bas gauche droite]*)
  let z = new zone in 
  z # id # set id;
  z # pos # set Vector.{x = float x; y = float y};
  z # rect # set Rect.{width = w; height = h};
  z # texture #set texture;
  z # haut # set haut;
  z # bas # set bas;
  z # gauche # set gauche;
  z # droite # set droite;
  z # layer # set 3;
  z #effect # set 1;
  Draw_system.register ( z:> drawable);
  Zonable_System.register (z :> zonable);
  z


let create_death id x y w h =
  let z = new zone in 
  z # id # set id;
  z # pos # set Vector.{x = float x; y = float y};
  z # rect # set Rect.{width = w; height = h};
  z # texture #set trans_black;
  z # layer # set 3;
  z #effect # set 2;
  z # haut # set true;
  z # bas # set false;
  z # gauche # set false;
  z # droite # set true;
  Draw_system.register ( z:> drawable);
  Zonable_System.register (z :> zonable);
  z



let create_tp_entree id sibling x y w h =
  let z = new zone in 
  z # pos # set Vector.{x = float x; y = float y};
  z # rect # set Rect.{width = w; height = h};
  z # texture # set trans_red;
  z # id # set id;
  z # layer # set 3;
  z # sibling # set sibling;
  z # effect # set 3;
  Draw_system.register ( z:> drawable);
  Zonable_System.register (z :> zonable);
  z