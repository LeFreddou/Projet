open Component_def
open System_def

let create id x y w h =
  let cam = new camera in 
  cam # id # set id;
  cam # pos # set Vector.{x = float x; y = float y};
  cam # rect # set Rect.{width = w; height = h};
  cam # layer # set 0;
  Draw_system.register(cam :> drawable);
  Move_system.register(cam :> movable);
  cam