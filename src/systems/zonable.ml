open Component_def

type t = zonable

let init _ = ()

let find_exit node (el : t Seq.t) =
  let a = Seq.filter_map (fun (e : t) -> if ((e#id#get) = node#sibling#get && e#effect#get = 4 )
     then Some e else None) el in
  if Seq.is_empty a then failwith "Error" 
  else Seq.take 1 a

let update_moov e1 e2 =     
  e1#haut#set e2#haut#get ;
  e1#bas#set e2#bas#get;
  e1#gauche#set e2#gauche#get;
  e1#droite#set e2#droite#get

let update _dt (el : t Seq.t) =
  let el1 = Seq.filter_map (fun (e1 : t) -> if e1#layer#get = 1 then Some e1 else None) el in
  let el2 = Seq.filter_map (fun (e2 : t) -> if e2#layer#get = 3 then Some e2 else None) el in
  Seq.iter (fun (e1:t) -> 
    let pos1 = e1#pos#get in
    let box1 = e1#rect#get in 
    Seq.iter (fun (e2 : t) -> 
      let pos2 = e2#pos#get in
      let box2 = e2#rect#get in 
      let s_pos, s_rect = Rect.mdiff pos2 box2 pos1 box1 in
      if Rect.has_origin s_pos s_rect 
      then begin
        match e2#effect#get with 
        1 -> update_moov e1 e2;
        |2 -> update_moov e1 e2;
        |3 -> let sorties = find_exit e2 el2 in
         Seq.iter (fun (sortie :t) -> 
          let n_pos = Vector.add sortie#pos#get 
                                (Vector.mult (1./.2.) Vector.{x = float (sortie#rect#get).width; y = float sortie#rect#get.height})
          in
          let n_pos = Vector.add n_pos (Vector.sub Vector.zero (Vector.mult (1./.2.) Vector.{x = float (e1#rect#get).width; y = float e1#rect#get.height})) 
          in e1#pos#set n_pos
          
          
          ) sorties
        |5 -> let depart = Vector.{x = 50. ; y = 460.} in e1#pos#set depart;
        update_moov e1 e2
        |_ -> ()
      
      
      
      end
      ) el2

    ) el1