open Ecs


module Key_system = System.Make (Key)

let () = System.register (module Key_system)

module Collisions_system = System.Make (Collisions)

let () = System.register (module Collisions_system)

module Zonable_System = System.Make (Zonable)

let () = System.register (module Zonable_System)

module Move_system = System.Make (Move)

let () = System.register (module Move_system)

module Draw_system = System.Make (Draw)

let () = System.register (module Draw_system)

module Next_lvl_syst = System.Make (Next_lvl)

let () = System.register (module Next_lvl_syst)