open Component_def


type t = movable 

let init _ = () 
let dt = 1000. /. 60.
let debug = ref false
let x = ref 0.
let y = ref 0.

let has_key, set_key, unset_key =
  let h = Hashtbl.create 16 in
  (fun s-> Hashtbl.mem h s),
  (fun s -> Hashtbl.replace h s ()), 
  (fun s-> Hashtbl.remove h s)


let update _dt el = 
  x := 0.;
  y := 0.;
  Seq.iter ( fun (e : t) -> 
  let () = match Gfx.poll_event () with
    KeyDown s -> set_key s; if false then Gfx.debug "%s\n%!"s;
    | KeyUp s -> unset_key s
    | _ -> () in 
    if (has_key "q" && e#moving#get && (e#gauche#get || !debug)) then x := !x -. 0.25;
    if (has_key "d" && e#moving#get && ((e#droite#get) || !debug)) then x := !x +. 0.25;
    if (has_key "z" && e#moving#get && ((e#haut#get) || !debug)) then y := !y -. 0.25;
    if (has_key "s" && e#moving#get && ((e#bas#get) || !debug)) then y := !y +. 0.25;
    if (has_key "a") then debug := false;
    if (has_key "e") then debug := true;
    if (has_key "r") then begin 
      e#pos#set Vector.{x = 50.;y= 460.};
      e#haut#set true;
      e#bas#set false;
      e#gauche#set false;
      e#droite#set true;
    end;
    let n_vel = if (!x != 0. && !y != 0.) then 
       Vector.mult (1./.(sqrt 2.)) Vector.{x =  !x ;y =  !y} 
       else Vector.{x =  !x ;y =  !y} in 
    e#velocity#set n_vel
  ) el