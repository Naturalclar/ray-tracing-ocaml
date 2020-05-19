open Base
open Stdio

let () =
  let width = 200
  and height = 100 in 
  let file = Out_channel.create "image.ppm" in
  let _ = Out_channel.fprintf file "P3\n%d %d\n255\n" width height in
  (Sequence.cartesian_product
    (Sequence.range ~stride:(-1) (height-1) (-1))
    (Sequence.range 0 width)
  )
  |> Sequence.iter
    ~f:(fun (j, i) -> 
      let r = Float.of_int(i) /. Float.of_int(width)
      and g = Float.of_int(j) /. Float.of_int(height)
      and b = 0.2 in
      let ir = Float.to_int(255.999 *. r) 
      and ig = Float.to_int(255.999 *. g)
      and ib = Float.to_int(255.999 *. b) in 
      Out_channel.fprintf file "%d %d %d\n" ir ig ib
    )
