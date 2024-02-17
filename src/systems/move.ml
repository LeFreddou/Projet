open Component_def

type t = movable

let init _ = () 
let dt = 1000. /. 60.

let update _dt el = 
  let autre = Seq.filter_map (fun (e:t) -> if e#layer#get != 0 then Some e else None) el in
  Seq.iter ( fun (e : t ) -> 
    let p = e#pos#get in 
    let v = e#velocity#get in 
    e#pos#set (Vector.add p (Vector.mult dt v));
    let Vector.{x; y} = e#pos#get in 
    if (x>=400. || y <=300.) then 
      let cam = Seq.filter_map (fun (e:t) -> if e#layer#get = 0 then Some e else None) el in
    Seq.iter (fun cam -> 
      if (y>300.) then
        cam#pos#set Vector.{x= x-.400.; y= cam#pos#get.y }
      else if x<400. then cam#pos#set Vector.{x= cam#pos#get.x; y=y-.300. }
      else cam#pos#set Vector.{x= x-.400.; y= y-.300. } )cam
    ) autre