(* rev e144b2c09f70728df82a34185b0b28a7af779fd6 *)

type color = {r: float; g: float; b: float}

type Color.t += ColorfulColor of color

let rgba ({r; g; b} : color) =
  ( Int.of_float ((r *. 65535.) +. 0.5)
  , Int.of_float ((g *. 65535.) +. 0.5)
  , Int.of_float ((b *. 65535.) +. 0.5)
  , 0xFFFF )

let () =
  Color.register (fun r_rgba -> function
    | ColorfulColor c -> rgba c | c -> r_rgba c )

let make_color col =
  let r, g, b, a = Color.rgba col in
  if a = 0 then ({r= 0.; g= 0.; b= 0.}, false)
  else
    let r = r * 0xFFFF in
    let r = r / a in
    let g = g * 0xFFFF in
    let g = g / a in
    let b = b * 0xFFFF in
    let b = b / a in
    ( { r= Float.of_int r /. 65535.
      ; g= Float.of_int g /. 65535.
      ; b= Float.of_int b /. 65535. }
    , true )

let d65 = (0.95047, 1.00000, 1.08883)

let sq v = v *. v

let cub v = v *. v *. v

(* L73 *)
let clamp01 v = Float.max 0. (Float.min v 1.)

(* L79 *)
let color_clamped {r; g; b} = {r= clamp01 r; g= clamp01 g; b= clamp01 b}

(* L359 *)
let color_hex col =
  Format.sprintf "#%02x%02x%02x"
    (Int.of_float ((col.r *. 255.0) +. 0.5))
    (Int.of_float ((col.g *. 255.0) +. 0.5))
    (Int.of_float ((col.b *. 255.0) +. 0.5))

(* L366 *)
let hex (scol : string) : color =
  let (format : (_, _, _, _, _, _) format6), factor =
    if String.length scol = 4 then ("#%1x%1x%1x", 1. /. 15.)
    else ("#%02x%02x%02x", 1. /. 255.)
  in
  let r, g, b = Scanf.sscanf scol format (fun r g b -> (r, g, b)) in
  { r= Float.of_int r *. factor
  ; g= Float.of_int g *. factor
  ; b= Float.of_int b *. factor }

(* L391 *)
let linearize v =
  if v <= 0.04045 then v /. 12.92 else Float.pow ((v +. 0.055) /. 1.055) 2.4

(* L399 *)
let color_linear_rgb col = (linearize col.r, linearize col.g, linearize col.b)

(* L426 *)
let delinearize v =
  if v <= 0.0031308 then 12.92 *. v
  else (1.055 *. Float.pow v (1.0 /. 2.4)) -. 0.055

(* L434 *)
let linear_rgb r g b = {r= delinearize r; g= delinearize g; b= delinearize b}

(* L472 *)
let xyz_to_linear_rgb x y z =
  ( (3.2409699419045214 *. x) -. (1.5373831775700935 *. y)
    -. (0.49861076029300328 *. z)
  , (-0.96924363628087983 *. x)
    +. (1.8759675015077207 *. y)
    +. (0.041555057407175613 *. z)
  , (0.055630079696993609 *. x)
    -. (0.20397695888897657 *. y) +. (1.0569715142428786 *. z) )

(* L479 *)
let linear_rgb_to_xyz r g b =
  ( (0.41239079926595948 *. r) +. (0.35758433938387796 *. g)
    +. (0.18048078840183429 *. b)
  , (0.21263900587151036 *. r) +. (0.71516867876775593 *. g)
    +. (0.072192315360733715 *. b)
  , (0.019330818715591851 *. r)
    +. (0.11919477979462599 *. g) +. (0.95053215224966058 *. b) )

(* L503 *)
let color_xyz col =
  let r, g, b = color_linear_rgb col in
  linear_rgb_to_xyz r g b

(* L507 *)
let xyz x y z =
  let r, g, b = xyz_to_linear_rgb x y z in
  linear_rgb r g b

(* L577 *)
let lab_f t =
  if t > 6. /. 29. *. 6. /. 29. *. 6. /. 29. then Float.pow t (1. /. 3.)
  else (t /. 3. *. 29. /. 6. *. 29. /. 6.) +. (4. /. 29.0)

(* L591 *)
let xyz_to_lab_white_ref x y z (wref0, wref1, wref2) =
  let fy = lab_f (y /. wref1) in
  let l = (1.16 *. fy) -. 0.16 in
  let a = 5. *. (lab_f (x /. wref0) -. fy) in
  let b = 2. *. (fy -. lab_f (z /. wref2)) in
  (l, a, b)

(* L584 *)
let xyz_to_lab x y z = xyz_to_lab_white_ref x y z d65

(* L599 *)
let lab_finv t =
  if t > 6. /. 29. then t *. t *. t
  else 3. *. 6. /. 29. *. 6. /. 29. *. (t -. (4. /. 29.))

(* L611 *)
let lab_to_xyz_white_ref l a b (wref0, wref1, wref2) =
  let l2 = (l +. 0.16) /. 1.16 in
  let x = wref0 *. lab_finv (l2 +. (a /. 5.)) in
  let y = wref1 *. lab_finv l2 in
  let z = wref2 *. lab_finv (l2 -. (b /. 2.)) in
  (x, y, z)

(* L606 *)
let lab_to_xyz l a b = lab_to_xyz_white_ref l a b d65

(* L620 *)
let color_lab col =
  let x, y, z = color_xyz col in
  xyz_to_lab x y z

(* L634 *)
let lab l a b =
  let x, y, z = lab_to_xyz l a b in
  xyz x y z

(* L781 *)
let color_blend_lab c1 c2 t =
  let l1, a1, b1 = color_lab c1 in
  let l2, a2, b2 = color_lab c2 in
  lab
    (l1 +. (t *. (l2 -. l1)))
    (a1 +. (t *. (a2 -. a1)))
    (b1 +. (t *. (b2 -. b1)))

(* L816 *)
let xyz_to_uv x y z =
  let denom = x +. (15.0 *. y) +. (3.0 *. z) in
  if denom = 0. then (0., 0.) else (4.0 *. x /. denom, 9.0 *. y /. denom)

(* L801 *)
let xyz_to_luv_white_ref x y z (wref0, wref1, wref2) =
  let l =
    if y /. wref1 <= 6.0 /. 29.0 *. (6.0 /. 29.0) *. (6.0 /. 29.0) then
      y /. wref1 *. (29.0 /. 3.0 *. 29.0 /. 3.0 *. 29.0 /. 3.0) /. 100.0
    else (1.16 *. Float.pow (y /. wref1) (1. /. 3.)) -. 0.16
  in
  let ubis, vbis = xyz_to_uv x y z in
  let un, vn = xyz_to_uv wref0 wref1 wref2 in
  (l, 13.0 *. l *. (ubis -. un), 13.0 *. l *. (vbis -. vn))

(* L794 *)
let xyz_to_luv x y z = xyz_to_luv_white_ref x y z d65

(* L832 *)
let luv_to_xyz_white_ref l u v (wref0, wref1, wref2) =
  let y =
    if l <= 0.08 then
      wref1 *. l *. 100.0 *. 3.0 /. 29.0 *. 3.0 /. 29.0 *. 3.0 /. 29.0
    else wref1 *. cub ((l +. 0.16) /. 1.16)
  in
  let un, vn = xyz_to_uv wref0 wref1 wref2 in
  if l <> 0. then
    let ubis = (u /. (13.0 *. l)) +. un in
    let vbis = (v /. (13.0 *. l)) +. vn in
    ( y *. 9.0 *. ubis /. (4.0 *. vbis)
    , y
    , y *. (12.0 -. (3.0 *. ubis) -. (20.0 *. vbis)) /. (4.0 *. vbis) )
  else (0., y, 0.)

(* L827 *)
let luv_to_xyz l u v = luv_to_xyz_white_ref l u v d65

(* L853 *)
let color_luv col =
  let x, y, z = color_xyz col in
  xyz_to_luv x y z

(* L860 *)
let color_luv_white_ref col wref =
  let x, y, z = color_xyz col in
  xyz_to_luv_white_ref x y z wref

(* L869 *)
let luv l u v =
  let x, y, z = luv_to_xyz l u v in
  xyz x y z

(* L876 *)
let luv_white_ref l u v wref =
  let x, y, z = luv_to_xyz_white_ref l u v wref in
  xyz x y z

(* L891 *)
let blend_luv c1 c2 t =
  let l1, u1, v1 = color_luv c1 in
  let l2, u2, v2 = color_luv c2 in
  luv
    (l1 +. (t *. (l2 -. l1)))
    (u1 +. (t *. (u2 -. u1)))
    (v1 +. (t *. (v2 -. v1)))

(* L981 *)
let luv_to_luv_lch l u v =
  let h =
    if abs_float (v -. u) > 0.0001 && abs_float u > 0.0001 then
      Float.rem ((57.29577951308232087721 *. Float.atan2 v u) +. 360.) 360.
    else 0.
  in
  (l, Float.sqrt (sq u +. sq v), h)

(* L996 *)
let color_luv_lch_white_ref c wref =
  let l, u, v = color_luv_white_ref c wref in
  luv_to_luv_lch l u v

(* L1008 *)
let luv_lch_to_luv l c h =
  let h = 0.01745329251994329576 *. h in
  (l, c *. Float.cos h, c *. Float.sin h)

(** HSLUV **)

let hs_luv_d65 = (0.95045592705167, 1.0, 1.089057750759878)

let m =
  [| (3.2409699419045214, -1.5373831775700935, -0.49861076029300328)
   ; (-0.96924363628087983, 1.8759675015077207, 0.041555057407175613)
   ; (0.055630079696993609, -0.20397695888897657, 1.0569715142428786) |]

let kappa = 903.2962962962963

let epsilon = 0.0088564516790356308

(* L162 *)
let get_bounds l =
  let ret = Array.make 6 (0., 0.) in
  let sub1 = Float.pow (l +. 16.) 3. /. 1560896. in
  let sub2 = if sub1 > epsilon then sub1 else l /. kappa in
  Array.iteri
    (fun i (m0, m1, m2) ->
      for k = 0 to 1 do
        let top1 = ((284517. *. m0) -. (94839. *. m2)) *. sub2 in
        let top2 =
          (((838422. *. m2) +. (769860. *. m1) +. (731718. *. m0)) *. l *. sub2)
          -. (769860. *. Float.of_int k *. l)
        in
        let bottom =
          (((632260. *. m2) -. (126452. *. m1)) *. sub2)
          +. (126452. *. Float.of_int k)
        in
        ret.((i * 2) + k) <- (top1 /. bottom, top2 /. bottom)
      done )
    m ;
  ret

(* L183 *)
let length_of_ray_until_intersect theta x y =
  y /. (Float.sin theta -. (x *. Float.cos theta))

(* L150 *)
let max_chroma_for_lh l h =
  let h_rad = h /. 360. *. Float.pi *. 2. in
  Array.fold_left
    (fun min_length (line0, line1) ->
      let length = length_of_ray_until_intersect h_rad line0 line1 in
      if length > 0. && length < min_length then length else min_length )
    Float.max_float (get_bounds l)

(* L18 *)
let luv_lch_to_hsluv l c h =
  let c = c *. 100. in
  let l = l *. 100. in
  let s =
    if l > 99.9999999 || l < 0.00000001 then 0.
    else
      let max = max_chroma_for_lh l h in
      c /. max *. 100.
  in
  (h, clamp01 (s /. 100.), clamp01 (l /. 100.))

(* L104 *)
let color_hsluv c =
  let l, c, h = color_luv_lch_white_ref c hs_luv_d65 in
  luv_lch_to_hsluv l c h

(* L124 *)
let color_distance_hsluv c1 c2 =
  let h1, s1, l1 = color_hsluv c1 in
  let h2, s2, l2 = color_hsluv c2 in
  Float.sqrt (sq ((h1 -. h2) /. 100.) +. sq (s1 -. s2) +. sq (l1 -. l2))
