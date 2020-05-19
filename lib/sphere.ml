open Vec3

type t = {center: Vec3.t; radius: float}

let create center radius = {center; radius}

let hit (r: Ray.t) ({center; radius}: t): float =
  let oc = r.origin -| center in
  let a = Vec3.length_square r.direction in 
  let half_b = Vec3.dot oc r.direction in
  let c = (Vec3.length_square oc) -. radius *. radius in
  let discriminant = half_b *. half_b -. a *. c in
  if discriminant < 0.
    then
      -1.
    else
      (-.half_b -. (sqrt discriminant)) /. a 
