open Component_def
open System_def

let create id x y w h color =
  let mur = new wall in 
  mur # pos # set Vector.{ x = float x; y = float y };
  mur # rect # set Rect.{width = w; height = h};
  mur # texture # set color;
  mur # id # set id;
  mur # layer # set 2;
  Collisions_system.register (mur :> collidable);
  Draw_system.register (mur :> drawable);
  mur