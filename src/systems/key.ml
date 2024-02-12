open Component_def


type t = movable 

let init _ = () 
let dt = 1000. /. 60.

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
    KeyDown s -> set_key s; Gfx.debug "%s\n%!"s;
    | KeyUp s -> unset_key s
    | _ -> () in 
    if (has_key "q" && not(e#moov_up_left#get)) then x := !x -. 0.25;
    if (has_key "d" && (e#moov_up_left#get)) then x := !x +. 0.25;
    if (has_key "z" && (e#moov_up_left#get)) then y := !y -. 0.25;
    if (has_key "s" && not(e#moov_up_left#get)) then y := !y +. 0.25;
    e#velocity#set Vector.{x =  !x ;y =  !y}
  ) el
