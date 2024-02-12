open Component_def 

type t = collidable

let init _ = () 

let update _dt (el : t Seq.t) =
let el1 = Seq.filter_map ( fun (e1 : t) ->if e1# layer #get  = 1 then Some e1 else None) el in
let el2 = Seq.filter_map ( fun (e2 : t) ->if e2# layer #get  = 2 then Some e2 else None) el in 
  Seq.iter
    ( fun (e1 : t) -> 
      let pos1 = e1#pos#get in
      let box1 = e1#rect#get in 
      let v1 = e1#velocity#get in
      Seq.iter
        (fun (e2 : t) ->
          let pos2 = e2#pos#get in 
          let box2 = e2#rect#get in
          let s_pos, s_rect = Rect.mdiff pos2 box2 pos1 box1 in 
          if 
            Rect.has_origin s_pos s_rect 
            && not (Vector.is_zero v1)
          then begin 
            let a = Vector.{ x = s_pos.x; y = 0.0 } in 
            let b = Vector.{ x = float s_rect.width +. s_pos.x; y = 0.0 } in 
            let c = Vector.{ x = 0.0; y = s_pos.y } in 
            let d = Vector.{ x = 0.0; y = float s_rect.height +. s_pos.y } in 
            let n = 
              List.fold_left 
                (fun min_v v -> 
                  if Vector.norm v <= Vector.norm min_v then v else min_v) 
                a [ b; c; d ]
            in 
            let pos1 = Vector.add pos1 n in 
            let s_pos, s_rect = Rect.mdiff pos2 box2 pos1 box1 in 
            if Rect.has_origin s_pos s_rect then begin
              Gfx.debug "%f, %f, %d x %d\n" s_pos.Vector.x s_pos.Vector.y
                s_rect.Rect.width s_rect.Rect.height
            end;
            e1#pos#set pos1;

            let n = Vector.normalize n in 
            (*let e = 0. in (*elasticité ici !!!!!!!!!!!!*)*)
            let j = -1. *. Vector.dot v1 n in 
            let new_v1 = Vector.add v1 (Vector.mult j n) in 
            e1#velocity#set new_v1
          end
        )
      el2)
  el1

let update _dt el = 
  for i=0 to 3 do 
    update _dt el 
  done