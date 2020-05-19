type t = { x: float; y: float; z: float}

let create x y z = {x;y;z}

let ( +| ) v1 v2 = {
  x= v1.x +. v2.x;
  y= v1.y +. v2.y;
  z= v1.z +. v2.z
}

let ( -| ) v1 v2 = {
  x= v1.x -. v2.x;
  y= v1.y -. v2.y;
  z= v1.z -. v2.z
}

let ( *| ) v s = {
  x= v.x *. s;
  y= v.y *. s;
  z= v.z *. s
}

let ( /| ) v s = {
  x= v.x /. s;
  y= v.y /. s;
  z= v.z /. s
}

let dot v1 v2 = 
  v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z

let length_square v =
  dot v v

let length v = 
  sqrt (length_square v)

let negate v =
  {x = (-.v.x); y=v.y; z=v.z}

let cross u v =
  {
    x = u.y *. v.z -. u.z *. v.y;
    y = u.z *. v.x -. u.x *. v.z;
    z = u.x *. v.y -. u.y *. v.x
  }

let lerp u v t = u *| (1. -. t) +| v *| t

let unit_vector v = 
  v /| length v
