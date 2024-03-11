open Component_def
open System_def

let create id x y w h color level =
  let j = new player in 
  j # pos # set Vector.{ x = float x; y = float y };
  j # rect # set Rect.{width = w; height = h};
  j # texture # set color;
  j # id # set id;
  j # layer # set 1;
  j # haut # set true;
  j#droite#set true;
  j#level#set level; 
  Key_system.register (j :> movable);
  Collisions_system.register (j :> collidable);
  Move_system.register (j :> movable);
  Draw_system.register (j :> drawable);
  Zonable_System.register (j:>zonable);
  Next_lvl_syst.register (j:>cancellable);
  j