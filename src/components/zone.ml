open Component_def
open System_def

let trans = Gfx.color 0 0 0 128



let create id x y w h effect =
  let z = new zone in 
  z # pos # set Vector.{x = float x; y = float y};
  z # rect # set Rect.{width = w; height = h};
  z # color # set trans;
  z # id # set id;
  z # layer # set 3;
  z #effect # set effect;
  Draw_system.register ( z:> drawable);
  Zonable_System.register (z :> zonable);
  z