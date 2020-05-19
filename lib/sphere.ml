open Vec3

type hit_record = {t: float; point: Vec3.t; normal: Vec3.t}

type t = {center: Vec3.t; radius: float}

let create center radius = {center; radius}
  

let hit (r: Ray.t) ({center; radius}: t): hit_record option =
  let t_min = 0. in
  let t_max = max_float in
  let oc = r.origin -| center in
  let a = Vec3.length_square r.direction in 
  let half_b = Vec3.dot oc r.direction in
  let c = (Vec3.length_square oc) -. radius *. radius in

  let hit_record_from_t t: hit_record option =
    if (t < t_max && t > t_min) then
      let point = Ray.at t r in
      Some {t; point; normal = (point -| center /| radius)}
    else
      None 
  in

  let discriminant = half_b *. half_b -. a *. c in
  if discriminant > 0.
    then
      let root = sqrt discriminant in
      let t1 = (-.half_b -. root) /. a in
      let t2 = (-.half_b +. root) /. a in
      match hit_record_from_t t1 with
      | Some hit_record -> Some hit_record
      | None ->
        match hit_record_from_t t2 with
      | Some hit_record -> Some hit_record
      | None -> None
    else
      None
