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

let ( *| ) v1 s = {
  x= v1.x *. s;
  y= v1.y *. s;
  z= v1.z *. s
}

let ( /| ) v1 s = {
  x= v1.x /. s;
  y= v1.y /. s;
  z= v1.z /. s
}

let dot v1 v2 = 
  v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z

let length_square v1 =
  dot v1 v1

let length v1 = 
  sqrt (length_square v1)
