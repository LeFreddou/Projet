open Component_def

type t = movable

let init _ = () 
let dt = 1000. /. 60.

let update _dt el = 
  Seq.iter ( fun (e : t ) -> 
    let p = e#pos#get in 
    let v = e#velocity#get in 
    e#pos#set (Vector.add p (Vector.mult dt v))
    ) el