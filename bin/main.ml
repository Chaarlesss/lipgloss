open Lipgloss
open Style

let light_dark =
  let has_dark_bg = has_dark_background stdin stdout in
  light_dark has_dark_bg

let subtle = light_dark ~light:"#D9DCCF" ~dark:"#383838"

let highlight = light_dark ~light:"#874BFD" ~dark:"#7D56F4"

let special = light_dark ~light:"#43BF6D" ~dark:"#73F59F"

let divider =
  new_style () |> set_string ["•"]
  |> padding HorizontalVertical (0, 1)
  |> foreground subtle |> string

let url l = new_style () |> foreground special |> render l

(* TABS *)

let active_tab_border =
  Border.create ~top:"─" ~bottom:" " ~left:"│" ~right:"│" ~top_left:"╭"
    ~top_right:"╮" ~bottom_left:"┘" ~bottom_right:"└" ()

let tab_border =
  Border.create ~top:"─" ~bottom:"─" ~left:"│" ~right:"│" ~top_left:"╭"
    ~top_right:"╮" ~bottom_left:"┴" ~bottom_right:"┴" ()

let tab =
  new_style () |> border tab_border All true
  |> border_foreground All highlight
  |> padding HorizontalVertical (0, 1)

let active_tab = tab |> border active_tab_border All true

let tab_gap = tab |> border_top false |> border_right false |> border_left false

(* Title *)

let title_style =
  new_style () |> margin_left 1 |> margin_right 5
  |> padding HorizontalVertical (0, 1)
  |> italic true
  |> foreground (color "#FFF7DB")
  |> set_string ["Lip Gloss"]

let desc_style = new_style () |> margin_top 1

let info_style =
  new_style ()
  |> border_style Border.normal_border
  |> border_top true
  |> border_foreground All subtle

(* Dialog *)

let dialog_box_style =
  new_style ()
  |> border Border.rounded_border All true
  |> border_foreground All (color "#874BFD")
  |> padding HorizontalVertical (1, 0)

let button_style =
  new_style ()
  |> foreground (color "#FFF7DB")
  |> background (color "#888B7E")
  |> padding HorizontalVertical (0, 3)
  |> margin_top 1

let active_button_style =
  button_style
  |> foreground (color "#FFF7DB")
  |> background (color "#F25D94")
  |> margin_right 2 |> underline true

(* Page *)

let doc_style = new_style () |> padding TopRightBottomLeft (1, 2, 1, 2)

let color_grid x_steps y_steps =
  let x0y0 = Colorful.hex "#F25D94" in
  let x1y0 = Colorful.hex "#EDFF82" in
  let x0y1 = Colorful.hex "#643AFF" in
  let x1y1 = Colorful.hex "#14F9D5" in
  let x0 =
    Array.init y_steps (fun i ->
        Colorful.blend_luv x0y0 x0y1 (Float.of_int i /. Float.of_int y_steps) )
  in
  let x1 =
    Array.init y_steps (fun i ->
        Colorful.blend_luv x1y0 x1y1 (Float.of_int i /. Float.of_int y_steps) )
  in
  let grid = Array.make_matrix y_steps x_steps "" in
  for x = 0 to y_steps - 1 do
    let y0 = x0.(x) in
    for y = 0 to x_steps - 1 do
      grid.(x).(y) <-
        Colorful.blend_luv y0 x1.(x) (Float.of_int y /. Float.of_int x_steps)
        |> Colorful.color_hex
    done
  done ;
  grid

let apply_gradient base input from_c to_c =
  let a, _ = Colorful.make_color to_c in
  let b, _ = Colorful.make_color from_c in
  let buffer = Buffer.create 4096 in
  String.iteri
    (fun i c ->
      let h =
        Colorful.blend_luv a b
          (Float.of_int i /. Float.of_int (String.length input - 1))
        |> Colorful.color_hex
      in
      let str = base |> foreground (color h) |> render [String.make 1 c] in
      Buffer.add_string buffer str )
    input ;
  Buffer.contents buffer

let _ =
  let width_terminal = 96 in
  let doc =
    let buffer = Buffer.create 4096 in
    let row =
      join_horizontal Position.top
        [ render ["Lip Gloss"] active_tab
        ; render ["Blush"] tab
        ; render ["Eye Shadow"] tab
        ; render ["Mascara"] tab
        ; render ["Foundation"] tab ]
    in
    let gap =
      render
        [ String.make
            (max 0 (width_terminal - Lipgloss.Style.string_width row - 2))
            ' ' ]
        tab_gap
    in
    let row = join_horizontal Position.bottom [row; gap] in
    Buffer.add_string buffer row ;
    Buffer.add_string buffer "\n\n" ;
    let title =
      let buffer = Buffer.create 1024 in
      let colors = color_grid 1 5 in
      Array.iteri
        (fun i v ->
          let offset = 2 in
          let c = color v.(0) in
          Buffer.add_string buffer
            ( title_style
            |> margin_left (i * offset)
            |> background c |> render [] ) ;
          if i < Array.length colors - 1 then Buffer.add_string buffer "\n" )
        colors ;
      Buffer.contents buffer
    in
    let desc =
      join_vertical Position.left
        [ render ["Style Definitions for Nice Terminal Layouts"] desc_style
        ; render
            [ "From Charm" ^ divider
              ^ url ["https://github.com/charmbracelet/lipgloss"] ]
            info_style ]
    in
    let row = join_horizontal Position.top [title; desc] in
    Buffer.add_string buffer row ;
    Buffer.add_string buffer "\n\n" ;
    let ok_button = render ["Yes"] active_button_style in
    let cancel_button = render ["Maybe"] button_style in
    let grad =
      apply_gradient (new_style ()) "Are you sure you want to eat marmalade?"
        (color "#EDFF82") (color "#F25D94")
    in
    let question =
      new_style () |> width 50
      |> align_horizontal Position.center
      |> render [grad]
    in
    let buttons = join_horizontal Position.top [ok_button; cancel_button] in
    let ui = join_vertical Position.center [question; buttons] in
    let dialog =
      place width_terminal 9 Position.Center Position.center
        (render [ui] dialog_box_style)
        ( Whitespace.create () |> Whitespace.chars "猫咪"
        |> Whitespace.style (new_style () |> foreground subtle) )
        render
    in
    Buffer.add_string buffer dialog ;
    Buffer.add_string buffer "\n\n" ;
    Buffer.contents buffer
  in
  print_string (render [doc] doc_style) ;
  print_newline ()
