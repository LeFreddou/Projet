open Component_def 

type t = cancellable

let init _ = ()

let update _dt (el : t Seq.t) = 
  let player = Seq.find (fun (e2 : t) -> if e2#layer#get =1 then true else false) el in
  let player = match player with 
  |Some p -> p 
  |_ -> failwith ("pas de joueur")

in
  if player#won#get then begin 
    
    (*destruction ancien niveau*)
    let all_obj =  Seq.filter_map (fun (e2 : t) -> if e2#layer#get !=1 then Some e2 else None) el in
    let l = ref [] in 
    Seq.iter(fun (obj:t) -> l := obj :: !l) all_obj;
    List.iter (fun (e1:t) -> e1#remove#get ()) !l;
    
    (*debut nouveau niveau*)
    player#level#set (player#level#get + 1);
    (*Load_lvl.load_lvl player#level#get;*)

    player#won#set false;
  end