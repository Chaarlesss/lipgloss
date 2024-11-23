(** COLOR **)

let foreground = "38"

let background = "48"

type color =
  | NoColor
  | ANSIColor of int
  | ANSI256Color of int
  | RGBColor of string

let sequence bg = function
  | NoColor ->
      ""
  | ANSIColor col ->
      let bg_mod c = if bg then c + 10 else c in
      if col < 8 then Format.sprintf "%d" (bg_mod col + 30)
      else Format.sprintf "%d" (bg_mod (col - 8) + 90)
  | ANSI256Color c ->
      let prefix = if bg then background else foreground in
      Format.sprintf "%s;5;%d" prefix c
  | RGBColor c ->
      let f = Colorful.hex c in
      let prefix = if bg then background else foreground in
      Format.sprintf "%s;2;%d;%d;%d" prefix
        (int_of_float (f.r *. 255.))
        (int_of_float (f.g *. 255.))
        (int_of_float (f.b *. 255.))

let ansi_hex =
  [| "#000000"
   ; "#800000"
   ; "#008000"
   ; "#808000"
   ; "#000080"
   ; "#800080"
   ; "#008080"
   ; "#c0c0c0"
   ; "#808080"
   ; "#ff0000"
   ; "#00ff00"
   ; "#ffff00"
   ; "#0000ff"
   ; "#ff00ff"
   ; "#00ffff"
   ; "#ffffff"
   ; "#000000"
   ; "#00005f"
   ; "#000087"
   ; "#0000af"
   ; "#0000d7"
   ; "#0000ff"
   ; "#005f00"
   ; "#005f5f"
   ; "#005f87"
   ; "#005faf"
   ; "#005fd7"
   ; "#005fff"
   ; "#008700"
   ; "#00875f"
   ; "#008787"
   ; "#0087af"
   ; "#0087d7"
   ; "#0087ff"
   ; "#00af00"
   ; "#00af5f"
   ; "#00af87"
   ; "#00afaf"
   ; "#00afd7"
   ; "#00afff"
   ; "#00d700"
   ; "#00d75f"
   ; "#00d787"
   ; "#00d7af"
   ; "#00d7d7"
   ; "#00d7ff"
   ; "#00ff00"
   ; "#00ff5f"
   ; "#00ff87"
   ; "#00ffaf"
   ; "#00ffd7"
   ; "#00ffff"
   ; "#5f0000"
   ; "#5f005f"
   ; "#5f0087"
   ; "#5f00af"
   ; "#5f00d7"
   ; "#5f00ff"
   ; "#5f5f00"
   ; "#5f5f5f"
   ; "#5f5f87"
   ; "#5f5faf"
   ; "#5f5fd7"
   ; "#5f5fff"
   ; "#5f8700"
   ; "#5f875f"
   ; "#5f8787"
   ; "#5f87af"
   ; "#5f87d7"
   ; "#5f87ff"
   ; "#5faf00"
   ; "#5faf5f"
   ; "#5faf87"
   ; "#5fafaf"
   ; "#5fafd7"
   ; "#5fafff"
   ; "#5fd700"
   ; "#5fd75f"
   ; "#5fd787"
   ; "#5fd7af"
   ; "#5fd7d7"
   ; "#5fd7ff"
   ; "#5fff00"
   ; "#5fff5f"
   ; "#5fff87"
   ; "#5fffaf"
   ; "#5fffd7"
   ; "#5fffff"
   ; "#870000"
   ; "#87005f"
   ; "#870087"
   ; "#8700af"
   ; "#8700d7"
   ; "#8700ff"
   ; "#875f00"
   ; "#875f5f"
   ; "#875f87"
   ; "#875faf"
   ; "#875fd7"
   ; "#875fff"
   ; "#878700"
   ; "#87875f"
   ; "#878787"
   ; "#8787af"
   ; "#8787d7"
   ; "#8787ff"
   ; "#87af00"
   ; "#87af5f"
   ; "#87af87"
   ; "#87afaf"
   ; "#87afd7"
   ; "#87afff"
   ; "#87d700"
   ; "#87d75f"
   ; "#87d787"
   ; "#87d7af"
   ; "#87d7d7"
   ; "#87d7ff"
   ; "#87ff00"
   ; "#87ff5f"
   ; "#87ff87"
   ; "#87ffaf"
   ; "#87ffd7"
   ; "#87ffff"
   ; "#af0000"
   ; "#af005f"
   ; "#af0087"
   ; "#af00af"
   ; "#af00d7"
   ; "#af00ff"
   ; "#af5f00"
   ; "#af5f5f"
   ; "#af5f87"
   ; "#af5faf"
   ; "#af5fd7"
   ; "#af5fff"
   ; "#af8700"
   ; "#af875f"
   ; "#af8787"
   ; "#af87af"
   ; "#af87d7"
   ; "#af87ff"
   ; "#afaf00"
   ; "#afaf5f"
   ; "#afaf87"
   ; "#afafaf"
   ; "#afafd7"
   ; "#afafff"
   ; "#afd700"
   ; "#afd75f"
   ; "#afd787"
   ; "#afd7af"
   ; "#afd7d7"
   ; "#afd7ff"
   ; "#afff00"
   ; "#afff5f"
   ; "#afff87"
   ; "#afffaf"
   ; "#afffd7"
   ; "#afffff"
   ; "#d70000"
   ; "#d7005f"
   ; "#d70087"
   ; "#d700af"
   ; "#d700d7"
   ; "#d700ff"
   ; "#d75f00"
   ; "#d75f5f"
   ; "#d75f87"
   ; "#d75faf"
   ; "#d75fd7"
   ; "#d75fff"
   ; "#d78700"
   ; "#d7875f"
   ; "#d78787"
   ; "#d787af"
   ; "#d787d7"
   ; "#d787ff"
   ; "#d7af00"
   ; "#d7af5f"
   ; "#d7af87"
   ; "#d7afaf"
   ; "#d7afd7"
   ; "#d7afff"
   ; "#d7d700"
   ; "#d7d75f"
   ; "#d7d787"
   ; "#d7d7af"
   ; "#d7d7d7"
   ; "#d7d7ff"
   ; "#d7ff00"
   ; "#d7ff5f"
   ; "#d7ff87"
   ; "#d7ffaf"
   ; "#d7ffd7"
   ; "#d7ffff"
   ; "#ff0000"
   ; "#ff005f"
   ; "#ff0087"
   ; "#ff00af"
   ; "#ff00d7"
   ; "#ff00ff"
   ; "#ff5f00"
   ; "#ff5f5f"
   ; "#ff5f87"
   ; "#ff5faf"
   ; "#ff5fd7"
   ; "#ff5fff"
   ; "#ff8700"
   ; "#ff875f"
   ; "#ff8787"
   ; "#ff87af"
   ; "#ff87d7"
   ; "#ff87ff"
   ; "#ffaf00"
   ; "#ffaf5f"
   ; "#ffaf87"
   ; "#ffafaf"
   ; "#ffafd7"
   ; "#ffafff"
   ; "#ffd700"
   ; "#ffd75f"
   ; "#ffd787"
   ; "#ffd7af"
   ; "#ffd7d7"
   ; "#ffd7ff"
   ; "#ffff00"
   ; "#ffff5f"
   ; "#ffff87"
   ; "#ffffaf"
   ; "#ffffd7"
   ; "#ffffff"
   ; "#080808"
   ; "#121212"
   ; "#1c1c1c"
   ; "#262626"
   ; "#303030"
   ; "#3a3a3a"
   ; "#444444"
   ; "#4e4e4e"
   ; "#585858"
   ; "#626262"
   ; "#6c6c6c"
   ; "#767676"
   ; "#808080"
   ; "#8a8a8a"
   ; "#949494"
   ; "#9e9e9e"
   ; "#a8a8a8"
   ; "#b2b2b2"
   ; "#bcbcbc"
   ; "#c6c6c6"
   ; "#d0d0d0"
   ; "#dadada"
   ; "#e4e4e4"
   ; "#eeeeee" |]

let ansi256_to_ansi_color = function
  | ANSI256Color c ->
      let h = Colorful.hex ansi_hex.(c) in
      let r = ref 0 in
      let md = ref Float.max_float in
      for i = 0 to 15 do
        let hb = Colorful.hex ansi_hex.(i) in
        let d = Colorful.color_distance_hsluv h hb in
        if d < !md then (
          md := d ;
          r := i )
      done ;
      ANSIColor !r
  | _ ->
      assert false

let hex_to_ansi256_color (c : Colorful.color) =
  let v2ci v =
    if v < 48. then 0
    else if v < 115. then 1
    else Int.of_float ((v -. 35.) /. 40.)
  in
  let r = v2ci (c.r *. 255.) in
  let g = v2ci (c.g *. 255.) in
  let b = v2ci (c.b *. 255.) in
  let ci = (36 * r) + (6 * g) + b in
  let i2cv = [|0x00; 0x5F; 0x87; 0xAF; 0xD7; 0xFF|] in
  let cr = i2cv.(r) in
  let cg = i2cv.(g) in
  let cb = i2cv.(b) in
  let average = (r + g + b) / 3 in
  let gray_idx = if average > 238 then 23 else (average - 3) / 10 in
  let gv = 8 + (10 * gray_idx) in
  let c2 : Colorful.color =
    { r= Float.of_int cr /. 255.
    ; g= Float.of_int cg /. 255.
    ; b= Float.of_int cb /. 255. }
  in
  let g2 : Colorful.color =
    { r= Float.of_int gv /. 255.
    ; g= Float.of_int gv /. 255.
    ; b= Float.of_int gv /. 255. }
  in
  let color_dist = Colorful.color_distance_hsluv c c2 in
  let gray_dist = Colorful.color_distance_hsluv c g2 in
  if color_dist <= gray_dist then ANSI256Color (16 + ci)
  else ANSI256Color (232 + gray_idx)

(** PROFILE **)

type profile = TrueColor | ANSI256 | ANSI | ASCII

let convert (c : color) : profile -> color = function
  | ASCII ->
      NoColor
  | p -> (
    match c with
    | ANSI256Color _ ->
        if p = ANSI then assert false else c
    | RGBColor v ->
        let h = Colorful.hex v in
        if p <> TrueColor then
          let ac = hex_to_ansi256_color h in
          if p = ANSI then ansi256_to_ansi_color ac else ac
        else c
    | ANSIColor _ | _ ->
        c )

let color (s : string) (profile : profile) : color =
  if s = "" then NoColor
  else
    let c =
      if String.starts_with ~prefix:"#" s then RGBColor s else assert false
    in
    convert c profile

(** TERMENV **)

let esc = '\x1b'

let bel = '\x07'

let csi = String.make 1 esc ^ "["

let ocs = String.make 1 esc ^ "]"

let st = String.make 1 esc ^ "\\"

type environ = OSEnviron

let get_env str = function
  | OSEnviron ->
      Sys.getenv_opt str |> Option.value ~default:""

let cli_color_forced env =
  let forced = get_env "CLICOLOR_FORCE" env in
  forced <> "" && forced <> "0"

let env_no_color env =
  get_env "NO_COLOR" env <> ""
  || (get_env "CLICOLOR" env = "0" && not (cli_color_forced env))

let rec string_contains ~sub = function
  | "" ->
      sub = "" (* the empty string contains itself *)
  | s ->
      String.starts_with ~prefix:sub s
      || string_contains ~sub (String.sub s 1 (String.length s - 1))

let color_profile env =
  let term = get_env "TERM" env in
  let color_term = get_env "COLORTERM" env in
  match String.lowercase_ascii color_term with
  | "24bit" | "truecolor" ->
      if
        String.starts_with ~prefix:"screen" term
        && get_env "TERM_PROGRAM" env <> "tmux"
      then ANSI256
      else TrueColor
  | "yes" | "true" ->
      ANSI256
  | _ -> (
    match term with
    | "alacritty" | "contour" | "wezterm" | "xterm-ghostty" | "xterm-kitty" ->
        TrueColor
    | "linux" ->
        ANSI
    | _ ->
        if string_contains ~sub:"256color" term then ANSI256
        else if string_contains ~sub:"color" term then ANSI
        else if string_contains ~sub:"ansi" term then ANSI
        else ASCII )

let env_color_profile env =
  if env_no_color env then ASCII
  else
    let p = color_profile env in
    if cli_color_forced env && p = ASCII then ANSI else p

(** OUTPUT **)

module Output = struct
  type t =
    { profile: profile
    ; w: out_channel
    ; environ: environ
    ; cache: bool
    ; fgColor: color
    ; bgColor: color }

  let new_output w =
    let environ = OSEnviron in
    let profile = env_color_profile environ in
    {profile; w; environ; cache= false; fgColor= NoColor; bgColor= NoColor}

  let output = new_output Stdlib.stdout

  let default_output () = output
end

(** STYLE **)

let reset_seq = "0"

let bold_seq = "1"

let faint_seq = "2"

let italic_seq = "3"

let underline_seq = "4"

let blink_seq = "5"

let reverse_seq = "7"

let cross_out_seq = "9"

let overline_seq = "53"

type style = {profile: profile; string: string; style: string list}

let create_style ?(profile = ANSI) ?(s = []) () =
  {profile; string= String.concat " " s; style= []}

let styled style s =
  match style.profile with
  | ASCII ->
      s
  | _ -> (
    match style.style with
    | [] ->
        s
    | l ->
        Format.sprintf "%s%sm%s%sm" csi
          (String.concat ";" (List.rev l))
          s (csi ^ reset_seq) )

let foreground color style =
  {style with style= sequence false color :: style.style}

let background color style =
  {style with style= sequence true color :: style.style}

let bold style = {style with style= bold_seq :: style.style}

let faint style = {style with style= faint_seq :: style.style}

let italic style = {style with style= italic_seq :: style.style}

let underline style = {style with style= underline_seq :: style.style}

let overline style = {style with style= overline_seq :: style.style}

let blink style = {style with style= blink_seq :: style.style}

let reverse style = {style with style= reverse_seq :: style.style}

let cross_out style = {style with style= cross_out_seq :: style.style}

let width _style = assert false
