(* L120 *)
let blends c1 c2 count =
  let dl = 1. /. Float.of_int (count + 1) in
  Array.init count (fun i ->
      Colorful.color_blend_lab c1 c2 (dl *. Float.of_int (i + 1))
      |> Colorful.color_clamped )
