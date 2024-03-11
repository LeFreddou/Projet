open Component_def 

type t = cancellable

let init _ = ()

let update _dt (el : t Seq.t) = 
  let player = Seq.filter_map (fun (e2 : t) -> if e2#layer#get =1 then Some e2 else None) el in
  if Seq.exists (fun (e : t) -> if e#won#get then true else false ) player then begin 
    
    (*destruction ancien niveau*)
    let all_obj =  Seq.filter_map (fun (e2 : t) -> if e2#layer#get !=1 then Some e2 else None) el in
    let l = ref [] in 
    Seq.iter(fun (obj:t) -> l := obj :: !l) all_obj;
    List.iter (fun (e1:t) -> e1#remove#get ()) !l;
    
    (*debut nouveau niveau*)
    (*Seq.iter(fun (e:t) ->e#level#set (e#level#get +1); Load_lvl.load_lvl e#level#get) player;*)


  Seq.iter (fun (e : t) -> e#won#set false) player
  end