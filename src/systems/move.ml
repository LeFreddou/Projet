open Component_def
type t = movable

let init _ = () 
let dt = 1000. /. 60.
let free_cam = false

let update _dt el = 
  let autre = Seq.filter_map (fun (e:t) -> if e#layer#get != 0 then Some e else None) el in
  Seq.iter ( fun (e : t ) -> 
    let p = e#pos#get in 
    let v = e#velocity#get in 
    e#pos#set (Vector.add p (Vector.mult dt v));
    let Vector.{x; y} = e#pos#get in 
    if (x>=400. || y <=300. || free_cam) then 
      let cam = Seq.find (fun (e:t) -> if e#layer#get = 0 then true else false) el in
      let cam = match cam with 
        |Some p -> p 
        |_ -> failwith ("pas de caméra")
      in
      if (y>300. && not(free_cam)) then
        cam#pos#set Vector.{x= x-.400.; y= cam#pos#get.y }
      else if (x<400. && not(free_cam)) then cam#pos#set Vector.{x= cam#pos#get.x; y=y-.300. }
      else cam#pos#set Vector.{x= x-.400.; y= y-.300. }
    ) autre