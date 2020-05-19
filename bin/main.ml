open Base
open Stdio
open Raytrace
open Raytrace.Vec3
open Stdlib



let ray_color (r: Ray.t) =
  let sphere = Sphere.create (Vec3.create 0. 0. (-1.)) 0.5 in
  match Sphere.hit r sphere with
    | Some hit_record ->
      let n = Vec3.unit_vector ((Ray.at hit_record.t r) -| (Vec3.create 0. 0. (-1.))) in
      let color = (n +| (Vec3.create 1. 1. 1.)) *| 0.5 in
      Vec3.create (color.x) (color.y) (color.z)
    | None -> 
      let unit_direction = r.direction in
      let t = 0.5 *. (unit_direction.y +. 1.0) in
      Vec3.lerp (Vec3.create 1. 1. 1.) (Vec3.create 0.5 0.7 1.0) t

let float_to_color f = Float.to_int(255.999 *. f)

let () =
  let aspect_ratio = 16.0 /. 9.0 in
  let width = 384 in
  let height = Float.to_int(Float.of_int(width) /. aspect_ratio) in
  let viewport_height = 2.0 in
  let viewport_width = aspect_ratio *. viewport_height in
  let focal_length = 1.0 in 

  let origin = Vec3.create 0. 0. 0. in
  let horizontal = Vec3.create viewport_width 0. 0. in
  let vertical = Vec3.create 0. viewport_height 0. in
  let lower_left_corner = origin -| horizontal /| 2. -| vertical /| 2. -| Vec3.create 0. 0. focal_length in
  let file = Out_channel.create "image.ppm" in
  let _ = Out_channel.fprintf file "P3\n%d %d\n255\n" width height in
  (Sequence.cartesian_product
    (Sequence.range ~stride:(-1) (height-1) (-1))
    (Sequence.range 0 width)
  )
  |> Sequence.iter
    ~f:(fun (j, i) -> 
      let u = Float.of_int(i) /. Float.of_int(width-1) in 
      let v = Float.of_int(j) /. Float.of_int(height-1) in
      let r = Ray.create origin (lower_left_corner +| horizontal *| u +| vertical *| v -| origin) in
      let color = ray_color r in
      Out_channel.fprintf file "%d %d %d\n" (float_to_color color.x) (float_to_color color.y) (float_to_color color.z)
    )
