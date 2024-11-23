open Lipgloss
open Style

let normal = create_color "#EEEEEE"

let subtle = create_adaptative_color ~light:"#D9DCCF" ~dark:"#383838" ()

let highlight = create_adaptative_color ~light:"#874BFD" ~dark:"#7D56F4" ()

let special = create_adaptative_color ~light:"#43BF6D" ~dark:"#73F59F" ()

let blends = Gamut.blends (Colorful.hex "#F25D94") (Colorful.hex "#EDFF82") 50

let base = new_style () |> foreground normal

let divider =
  new_style () |> set_string ["•"]
  |> padding HorizontalVertical (0, 1)
  |> foreground subtle |> string

let url l = new_style () |> foreground special |> render l

(* TABS *)

let active_tab_border =
  create_border ~top:"─" ~bottom:" " ~left:"│" ~right:"│" ~top_left:"╭"
    ~top_right:"╮" ~bottom_left:"┘" ~bottom_right:"└" ()

let tab_border =
  create_border ~top:"─" ~bottom:"─" ~left:"│" ~right:"│" ~top_left:"╭"
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
  |> foreground (Color "#FFF7DB")
  |> set_string ["Lip Gloss"]

let desc_style = base |> margin_top 1

let info_style =
  base
  |> border_style (normal_border ())
  |> border_top true
  |> border_foreground All subtle

(* Dialog *)

let _dialog_box_style =
  new_style ()
  |> border (rounded_border ()) All true
  |> border_foreground All (create_color "#874BFD")
  |> padding HorizontalVertical (0, 1)

let button_style =
  new_style ()
  |> foreground (create_color "#FFF7DB")
  |> background (create_color "#888B7E")
  |> padding HorizontalVertical (0, 3)
  |> margin_top 1

let active_button_style =
  button_style
  |> foreground (create_color "#FFF7DB")
  |> background (create_color "#F25D94")
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

let rainbow base str colors =
  let buffer = Buffer.create 4096 in
  String.iteri
    (fun i c ->
      let color = colors.(i mod Array.length colors) in
      let str =
        base
        |> foreground (create_color (Colorful.color_hex color))
        |> render [String.make 1 c]
      in
      Buffer.add_string buffer str )
    str ;
  Buffer.contents buffer

let _ =
  (* BUGS: padding is doing shit with the spaces on top and bottom *)
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
          let c = create_color v.(0) in
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
    let question =
      new_style () |> width 50
      |> align_horizontal Position.center
      |> render
           [ rainbow (new_style ()) "Are you sure you want to eat marmalade?"
               blends ]
    in
    let buttons = join_horizontal Position.top [ok_button; cancel_button] in
    let ui = join_vertical Position.center [question; buttons] in
    Buffer.add_string buffer ui ;
    Buffer.add_string buffer "\n\n" ;
    Buffer.contents buffer
  in
  print_string (render [doc] doc_style) ;
  print_newline ()
