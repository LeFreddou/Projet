open Component_def
open System_def

let trans_black = Gfx.color 0 0 0 128
let trans_red = Gfx.color 255 0 0 128
let trans_blue = Gfx.color 0 0 255 128
let black = Gfx.color 0 0 0 255



let create id x y w h effect =
  let z = new zone in 
  z # pos # set Vector.{x = float x; y = float y};
  z # rect # set Rect.{width = w; height = h};
  let () = match effect with
  4 ->z#color#set trans_blue 
  |5 -> z#color#set black
  |_ -> z # color # set trans_black
in 
  z # id # set id;
  z # layer # set 3;
  z #effect # set effect;
  Draw_system.register ( z:> drawable);
  Zonable_System.register (z :> zonable);
  z

let create_tp_entree id sibling x y w h =
  let z = new zone in 
  z # pos # set Vector.{x = float x; y = float y};
  z # rect # set Rect.{width = w; height = h};
  z # color # set trans_red;
  z # id # set id;
  z # layer # set 3;
  z # sibling # set sibling;
  z # effect # set 3;
  Draw_system.register ( z:> drawable);
  Zonable_System.register (z :> zonable);
  z