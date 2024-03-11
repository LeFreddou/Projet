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
        |2 ->
         let depart = Vector.{x = 50. ; y = 460.} in
         e1#pos#set depart;
         update_moov e1 e2;
         let cameras = Seq.filter_map (fun (e1 : t) -> if e1#layer#get = 0 then Some e1 else None) el in
         Seq.iter (fun cam -> cam#pos#set Vector.zero) cameras
        |3 -> let sorties = find_exit e2 el2 in
         Seq.iter (fun (sortie :t) -> 
          let n_pos = Vector.add (Vector.add sortie#pos#get 
                                (Vector.mult (1./.2.) Vector.{x = float (sortie#rect#get).width; y = float sortie#rect#get.height}))
                                (Vector.sub Vector.zero (Vector.mult (1./.2.) Vector.{x = float (e1#rect#get).width; y = float e1#rect#get.height}))
          in
          e1#pos#set n_pos
          ) sorties
        |5 -> if not(e2#iced#get) then begin 
              e2#haut#set e1#haut#get;
              e2#bas#set e1#bas#get;
              e2#gauche#set e1#gauche#get; 
              e2#droite#set e1#droite#get;
              
              let has_key, set_key, unset_key =
                let h = Hashtbl.create 16 in
                (fun s-> Hashtbl.mem h s),
                (fun s -> Hashtbl.replace h s ()), 
                (fun s-> Hashtbl.remove h s) in 
              let () = match Gfx.poll_event () with
                KeyDown s -> set_key s; Gfx.debug "%s\n%!"s;
                | KeyUp s -> unset_key s
                | _ -> () in 
              e2#in_haut#set (has_key "z");
              e2#in_bas#set (has_key "s");
              e2#in_gauche#set (has_key "q");
              e2#in_droite#set (has_key "d");
              e1#haut#set false;
              e1#bas#set false;
              e1#droite#set false;
              e1#gauche#set false;
              Gfx.debug "haut :%b bas: %b gauche : %b droite : %b \n%!" e2#in_haut#get e2#in_bas#get e2#in_gauche#get e2#in_droite#get;
              end;
              let x = ref 0. in 
              let y = ref 0. in 
              if e2#in_haut#get then y := !y -. 0.25;
              if e2#in_bas#get then y := !y +. 0.25;
              if e2#in_gauche#get then x := !x -. 0.25;
              if e2#in_droite#get then x := !x +. 0.25;
              let n_vel = if (!x != 0. && !y != 0.) then 
                Vector.mult (1./.(sqrt 2.)) Vector.{x =  !x ;y =  !y} 
                else Vector.{x =  !x ;y =  !y} in 
            
              e1#velocity#set n_vel;
              e2#iced#set true
        |6 -> e1#won#set true
        |_ -> ()
      end
      else 
        if e2#iced#get = true then begin
          e1#haut#set e2#haut#get;
          e1#bas#set e2#bas#get;
          e1#droite#set e2#droite#get;
          e1#gauche#set e2#gauche#get;
          e2#haut#set false;
          e2#bas#set false;
          e2#gauche#set false;
          e2#droite#set false

        end

      ) el2
    ) el1