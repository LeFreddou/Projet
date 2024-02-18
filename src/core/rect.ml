open Vector

type t = { width : int; height : int }


let mdiff p1 r1 p2 r2 =
  (* We use the Minkowski difference of Box1 and Box2 *)
  let x = p1.x -. p2.x -. float r2.width in 
  let y = p1.y -. p2.y -. float r2.height in 
  let w = r2.width + r1.width in 
  let h = r2.height + r1.height in
  ({x;y}, {width = w; height = h})

let has_origin v r =
  v.x < 0.0
  && v.x +. float r.width > 0.0
  && v.y < 0.0
  && v.y +. float r.height > 0.0

let intersect v1 r1 v2 r2 =
  let v, r = mdiff v1 r1 v2 r2 in
  has_origin v r