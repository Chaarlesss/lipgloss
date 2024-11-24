(** COLOR **)

type Color.t += NoColor

let () =
  Color.register (fun rgba -> function
    | NoColor -> (0, 0, 0, 0xFFFF) | c -> rgba c )

let color s =
  if String.starts_with ~prefix:"#" s then
    Colorful.ColorfulColor (Colorful.hex s)
  else assert false

type light_dark_func = light:string -> dark:string -> Color.t

let light_dark is_dark : light_dark_func =
 fun ~light ~dark -> if is_dark then color dark else color light

module Border = struct
  type t =
    { top: string
    ; bottom: string
    ; left: string
    ; right: string
    ; top_left: string
    ; top_right: string
    ; bottom_left: string
    ; bottom_right: string
    ; middle_left: string
    ; middle_right: string
    ; middle: string
    ; middle_top: string
    ; middle_bottom: string }

  let no_border =
    { top= ""
    ; bottom= ""
    ; left= ""
    ; right= ""
    ; top_left= ""
    ; top_right= ""
    ; bottom_left= ""
    ; bottom_right= ""
    ; middle_left= ""
    ; middle_right= ""
    ; middle= ""
    ; middle_top= ""
    ; middle_bottom= "" }

  let create ?(top = "") ?(bottom = "") ?(left = "") ?(right = "")
      ?(top_left = "") ?(top_right = "") ?(bottom_left = "")
      ?(bottom_right = "") ?(middle_left = "") ?(middle_right = "")
      ?(middle = "") ?(middle_top = "") ?(middle_bottom = "") () =
    { top
    ; bottom
    ; left
    ; right
    ; top_left
    ; top_right
    ; bottom_left
    ; bottom_right
    ; middle_left
    ; middle_right
    ; middle
    ; middle_top
    ; middle_bottom }

  let normal_border =
    { top= "─"
    ; bottom= "─"
    ; left= "│"
    ; right= "│"
    ; top_left= "┌"
    ; top_right= "┐"
    ; bottom_left= "└"
    ; bottom_right= "┘"
    ; middle_left= "├"
    ; middle_right= "┤"
    ; middle= "┼"
    ; middle_top= "┬"
    ; middle_bottom= "┴" }

  let rounded_border =
    { top= "─"
    ; bottom= "─"
    ; left= "│"
    ; right= "│"
    ; top_left= "╭"
    ; top_right= "╮"
    ; bottom_left= "╰"
    ; bottom_right= "╯"
    ; middle_left= "├"
    ; middle_right= "┤"
    ; middle= "┼"
    ; middle_top= "┬"
    ; middle_bottom= "┴" }
end

module Position : sig
  type horizontal

  type vertical

  type _ t =
    | Top : vertical t
    | Bottom : vertical t
    | Left : horizontal t
    | Right : horizontal t
    | Center : _ t
    | Custom : float -> _ t

  val value : _ t -> float

  val top : vertical t

  val bottom : vertical t

  val left : horizontal t

  val right : horizontal t

  val center : _ t
end = struct
  type horizontal

  type vertical

  type _ t =
    | Top : vertical t
    | Bottom : vertical t
    | Left : horizontal t
    | Right : horizontal t
    | Center : _ t
    | Custom : float -> _ t

  let value : type a. a t -> float = function
    | Top ->
        0.
    | Bottom ->
        1.
    | Left ->
        0.
    | Right ->
        1.
    | Center ->
        0.5
    | Custom f ->
        min 1. (max 0. f)

  let top = Top

  let bottom = Bottom

  let left = Left

  let right = Right

  let center = Center
end

let string_width str =
  String.split_on_char '\n' str
  |> List.fold_left (fun x str -> max x (Ansi.string_width str)) 0

type _ prop_key =
  | BoldKey : bool prop_key
  | ItalicKey : bool prop_key
  | UnderlineKey : bool prop_key
  | StrikethroughKey : bool prop_key
  | ReverseKey : bool prop_key
  | BlinkKey : bool prop_key
  | FaintKey : bool prop_key
  | UnderlineSpacesKey : unit prop_key
  | StrikethroughSpacesKey : unit prop_key
  | ColorWhitespaceKey : bool prop_key
  | ForegroundKey : Color.t prop_key
  | BackgroundKey : Color.t prop_key
  | WidthKey : int prop_key
  | HeightKey : int prop_key
  | AlignHorizontalKey : Position.horizontal Position.t prop_key
  | AlignVerticalKey : Position.vertical Position.t prop_key
  | PaddingTopKey : int prop_key
  | PaddingRightKey : int prop_key
  | PaddingBottomKey : int prop_key
  | PaddingLeftKey : int prop_key
  | MarginTopKey : int prop_key
  | MarginRightKey : int prop_key
  | MarginBottomKey : int prop_key
  | MarginLeftKey : int prop_key
  | MarginBackgroundKey : Color.t prop_key
  | BorderStyleKey : Border.t prop_key
  | BorderTopKey : bool prop_key
  | BorderRightKey : bool prop_key
  | BorderBottomKey : bool prop_key
  | BorderLeftKey : bool prop_key
  | BorderTopForegroundKey : Color.t prop_key
  | BorderRightForegroundKey : Color.t prop_key
  | BorderBottomForegroundKey : Color.t prop_key
  | BorderLeftForegroundKey : Color.t prop_key
  | BorderTopBackgroundKey : Color.t prop_key
  | BorderRightBackgroundKey : Color.t prop_key
  | BorderBottomBackgroundKey : Color.t prop_key
  | BorderLeftBackgroundKey : Color.t prop_key
  | InlineKey : unit prop_key
  | MaxWidthKey : unit prop_key
  | MaxHeightKey : unit prop_key
  | TabWidthKey : unit prop_key
  | TransformKey : unit prop_key

let prop_key_to_int (k : _ prop_key) : int = Obj.magic k |> Int.shift_left 1

module Props = struct
  type t = int64

  let prop_key_to_bit (k : _ prop_key) : int64 =
    prop_key_to_int k |> Int64.of_int

  let set (p : t) (k : _ prop_key) : t = Int64.logor p (prop_key_to_bit k)

  let unset (p : t) (k : _ prop_key) : t =
    Int64.logand p (Int64.lognot (prop_key_to_bit k))

  let has (p : t) (k : _ prop_key) : bool =
    Int64.logand p (prop_key_to_bit k) <> 0L
end

type style =
  { props: Props.t
  ; value: string
  ; attrs: int
  ; fg_color: Color.t
  ; bg_color: Color.t
  ; width: int
  ; height: int
  ; align_horizontal: Position.horizontal Position.t
  ; align_vertical: Position.vertical Position.t
  ; padding_top: int
  ; padding_right: int
  ; padding_bottom: int
  ; padding_left: int
  ; margin_top: int
  ; margin_right: int
  ; margin_bottom: int
  ; margin_left: int
  ; margin_bg_color: Color.t
  ; border_style: Border.t
  ; borderTopFgColor: Color.t
  ; borderRightFgColor: Color.t
  ; borderBottomFgColor: Color.t
  ; borderLeftFgColor: Color.t
  ; borderTopBgColor: Color.t
  ; borderRightBgColor: Color.t
  ; borderBottomBgColor: Color.t
  ; borderLeftBgColor: Color.t }

let new_style () =
  { props= Int64.zero
  ; value= ""
  ; attrs= 0
  ; fg_color= NoColor
  ; bg_color= NoColor
  ; width= 0
  ; height= 0
  ; align_horizontal= Position.Left
  ; align_vertical= Position.Top
  ; padding_top= 0
  ; padding_right= 0
  ; padding_bottom= 0
  ; padding_left= 0
  ; margin_top= 0
  ; margin_right= 0
  ; margin_bottom= 0
  ; margin_left= 0
  ; margin_bg_color= NoColor
  ; border_style= Border.no_border
  ; borderTopFgColor= NoColor
  ; borderRightFgColor= NoColor
  ; borderBottomFgColor= NoColor
  ; borderLeftFgColor= NoColor
  ; borderTopBgColor= NoColor
  ; borderRightBgColor= NoColor
  ; borderBottomBgColor= NoColor
  ; borderLeftBgColor= NoColor }

let set_string strs style =
  let value = String.concat " " strs in
  {style with value}

let value style = style.value

let set_bool_key (key : bool prop_key) (value : bool) style =
  if value then {style with attrs= Int.logor style.attrs (prop_key_to_int key)}
  else
    {style with attrs= Int.logand style.attrs (Int.lognot (prop_key_to_int key))}

(** SET **)

let set (type a) (key : a prop_key) (value : a) style =
  let style =
    match key with
    | BoldKey ->
        set_bool_key key value style
    | ItalicKey ->
        set_bool_key key value style
    | UnderlineKey ->
        set_bool_key key value style
    | StrikethroughKey ->
        set_bool_key key value style
    | ReverseKey ->
        set_bool_key key value style
    | BlinkKey ->
        set_bool_key key value style
    | FaintKey ->
        set_bool_key key value style
    | ColorWhitespaceKey ->
        set_bool_key key value style
    | ForegroundKey ->
        {style with fg_color= value}
    | BackgroundKey ->
        {style with bg_color= value}
    | WidthKey ->
        {style with width= max 0 value}
    | HeightKey ->
        {style with height= max 0 value}
    | AlignHorizontalKey ->
        {style with align_horizontal= value}
    | AlignVerticalKey ->
        {style with align_vertical= value}
    | PaddingTopKey ->
        {style with padding_top= max 0 value}
    | PaddingRightKey ->
        {style with padding_right= max 0 value}
    | PaddingBottomKey ->
        {style with padding_bottom= max 0 value}
    | PaddingLeftKey ->
        {style with padding_left= max 0 value}
    | MarginTopKey ->
        {style with margin_top= max 0 value}
    | MarginRightKey ->
        {style with margin_right= max 0 value}
    | MarginBottomKey ->
        {style with margin_bottom= max 0 value}
    | MarginLeftKey ->
        {style with margin_left= max 0 value}
    | MarginBackgroundKey ->
        {style with margin_bg_color= value}
    | BorderStyleKey ->
        {style with border_style= value}
    | BorderTopKey ->
        set_bool_key key value style
    | BorderRightKey ->
        set_bool_key key value style
    | BorderBottomKey ->
        set_bool_key key value style
    | BorderLeftKey ->
        set_bool_key key value style
    | BorderTopForegroundKey ->
        {style with borderTopFgColor= value}
    | BorderRightForegroundKey ->
        {style with borderRightFgColor= value}
    | BorderBottomForegroundKey ->
        {style with borderBottomFgColor= value}
    | BorderLeftForegroundKey ->
        {style with borderLeftFgColor= value}
    | BorderTopBackgroundKey ->
        {style with borderTopBgColor= value}
    | BorderRightBackgroundKey ->
        {style with borderRightBgColor= value}
    | BorderBottomBackgroundKey ->
        {style with borderBottomBgColor= value}
    | BorderLeftBackgroundKey ->
        {style with borderLeftBgColor= value}
    | _ ->
        assert false
  in
  {style with props= Props.set style.props key}

type (_, _) sides =
  | All : ('a, 'a) sides
  | HorizontalVertical : ('a * 'a, 'a) sides
  | TopVerticalBottom : ('a * 'a * 'a, 'a) sides
  | TopRightBottomLeft : ('a * 'a * 'a * 'a, 'a) sides

let which_sides (type a b) (sides : (a, b) sides) (k : b -> b -> b -> b -> 'c) :
    a -> 'c =
  match sides with
  | All ->
      fun i0 -> k i0 i0 i0 i0
  | HorizontalVertical ->
      fun (i0, i1) -> k i0 i1 i0 i1
  | TopVerticalBottom ->
      fun (i0, i1, i2) -> k i0 i1 i2 i1
  | TopRightBottomLeft ->
      fun (i0, i1, i2, i3) -> k i0 i1 i2 i3

let bold v style = set BoldKey v style

let italic v style = set ItalicKey v style

let underline v style = set UnderlineKey v style

let strikethrough v style = set StrikethroughKey v style

let reverse v style = set ReverseKey v style

let blink v style = set BlinkKey v style

let faint v style = set FaintKey v style

let foreground c style = set ForegroundKey c style

let background c style = set BackgroundKey c style

let width i style = set WidthKey i style

let height i style = set HeightKey i style

let align_horizontal p style = set AlignHorizontalKey p style

let align_vertical p style = set AlignVerticalKey p style

let padding sides =
  which_sides sides (fun top right bottom left style ->
      set PaddingTopKey top style
      |> set PaddingRightKey right
      |> set PaddingBottomKey bottom
      |> set PaddingLeftKey left )

let margin sides =
  which_sides sides (fun top right bottom left style ->
      set MarginTopKey top style |> set MarginRightKey right
      |> set MarginBottomKey bottom |> set MarginLeftKey left )

let margin_left i style = set MarginLeftKey i style

let margin_right i style = set MarginRightKey i style

let margin_top i style = set MarginTopKey i style

let margin_bottom i style = set MarginBottomKey i style

let margin_color c style = set MarginBackgroundKey c style

let border border sides v style =
  set BorderStyleKey border style
  |> which_sides sides
       (fun top right bottom left style ->
         set BorderTopKey top style |> set BorderRightKey right
         |> set BorderBottomKey bottom |> set BorderLeftKey left )
       v

let border_style border style = set BorderStyleKey border style

let border_top v style = set BorderTopKey v style

let border_right v style = set BorderRightKey v style

let border_bottom v style = set BorderBottomKey v style

let border_left v style = set BorderLeftKey v style

let border_foreground sides v style =
  which_sides sides
    (fun top right bottom left style ->
      set BorderTopForegroundKey top style
      |> set BorderRightForegroundKey right
      |> set BorderBottomForegroundKey bottom
      |> set BorderLeftForegroundKey left )
    v style

let border_top_foreground c style = set BorderTopForegroundKey c style

let border_right_foreground c style = set BorderRightForegroundKey c style

let border_bottom_foreground c style = set BorderBottomForegroundKey c style

let border_left_foreground c style = set BorderLeftForegroundKey c style

(** GET **)

let is_set prop_key style = Props.has style.props prop_key

let get_as_bool (key : bool prop_key) default_value style =
  if is_set key style then Int.logand style.attrs (prop_key_to_int key) <> 0
  else default_value

let get_as_color (key : Color.t prop_key) style =
  if not (is_set key style) then NoColor
  else
    match key with
    | ForegroundKey ->
        style.fg_color
    | BackgroundKey ->
        style.bg_color
    | MarginBackgroundKey ->
        style.margin_bg_color
    | BorderTopForegroundKey ->
        style.borderTopFgColor
    | BorderRightForegroundKey ->
        style.borderRightFgColor
    | BorderBottomForegroundKey ->
        style.borderBottomFgColor
    | BorderLeftForegroundKey ->
        style.borderLeftFgColor
    | BorderTopBackgroundKey ->
        style.borderTopBgColor
    | BorderRightBackgroundKey ->
        style.borderRightBgColor
    | BorderBottomBackgroundKey ->
        style.borderBottomBgColor
    | BorderLeftBackgroundKey ->
        style.borderLeftBgColor

let get_as_int (key : int prop_key) style =
  if not (is_set key style) then 0
  else
    match key with
    | WidthKey ->
        style.width
    | HeightKey ->
        style.height
    | PaddingTopKey ->
        style.padding_top
    | PaddingRightKey ->
        style.padding_right
    | PaddingBottomKey ->
        style.padding_bottom
    | PaddingLeftKey ->
        style.padding_left
    | MarginTopKey ->
        style.margin_top
    | MarginRightKey ->
        style.margin_right
    | MarginBottomKey ->
        style.margin_bottom
    | MarginLeftKey ->
        style.margin_left

let get_as_position (type a) (key : a Position.t prop_key) style : a Position.t
    =
  if not (is_set key style) then Position.Custom 0.
  else
    match key with
    | AlignHorizontalKey ->
        style.align_horizontal
    | AlignVerticalKey ->
        style.align_vertical

let get_border_style style =
  if is_set BorderStyleKey style then style.border_style else Border.no_border

let get_lines (s : string) : string list * int =
  (* TODO: replace tabs? *)
  let lines = String.split_on_char '\n' s in
  let widest =
    List.fold_left
      (fun acc line ->
        let w = Ansi.string_width line in
        max acc w )
      0 lines
  in
  (lines, widest)

(** QUERY **)

let has_dark_background _ _ = true

(** WRITER **)

let writer =
  ColorProfile.Writer.create Format.std_formatter (fun str ->
      Sys.getenv_opt str |> Option.value ~default:"" )

(** BORDER **)

let runes str =
  Uutf.String.fold_utf_8
    (fun l _ -> function
      | `Malformed _ ->
          failwith "malformed string"
      | `Uchar c ->
          c :: l )
    [] str
  |> List.rev |> Array.of_list

let string rune =
  let buffer = Buffer.create 8 in
  Buffer.add_utf_8_uchar buffer rune ;
  Buffer.contents buffer

let render_horizontal_edge left middle right width =
  let middle = if middle = "" then " " else middle in
  let left_width = Ansi.string_width left in
  let right_width = Ansi.string_width right in
  let runes = runes middle in
  let j = ref 0 in
  let i = ref (left_width + right_width) in
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer left ;
  while !i < width + right_width do
    Buffer.add_utf_8_uchar buffer runes.(!j) ;
    incr j ;
    if !j >= Array.length runes then j := 0 ;
    i := !i + Ansi.string_width (string runes.(!j))
  done ;
  Buffer.add_string buffer right ;
  Buffer.contents buffer

let get_first_rune_as_string str =
  if str = "" then ""
  else String.get_utf_8_uchar str 0 |> Uchar.utf_decode_uchar |> string

let max_rune_width str =
  Uutf.String.fold_utf_8
    (fun x _ -> function
      | `Malformed _ ->
          failwith "max_run_width"
      | `Uchar uc ->
          max x (Uucp.Break.tty_width_hint uc) )
    0 str

let style_border _ border fg bg =
  if fg = NoColor && bg = NoColor then border
  else
    let style = Ansi.Style.create () in
    let style =
      if fg <> NoColor then Ansi.Style.foreground_color fg style else style
    in
    let style =
      if bg <> NoColor then Ansi.Style.background_color bg style else style
    in
    Ansi.Style.styled border style

let apply_border style str =
  let top_set = is_set BorderTopKey style in
  let right_set = is_set BorderRightKey style in
  let bottom_set = is_set BorderBottomKey style in
  let left_set = is_set BorderLeftKey style in
  let border = get_border_style style in
  let has_top, has_right, has_bottom, has_left =
    if
      border != Border.no_border
      && not (top_set || right_set || bottom_set || left_set)
    then (true, true, true, true)
    else
      ( get_as_bool BorderTopKey false style
      , get_as_bool BorderRightKey false style
      , get_as_bool BorderBottomKey false style
      , get_as_bool BorderLeftKey false style )
  in
  let top_fg = get_as_color BorderTopForegroundKey style in
  let right_fg = get_as_color BorderRightForegroundKey style in
  let bottom_fg = get_as_color BorderBottomForegroundKey style in
  let left_fg = get_as_color BorderLeftForegroundKey style in
  let top_bg = get_as_color BorderTopBackgroundKey style in
  let right_bg = get_as_color BorderRightBackgroundKey style in
  let bottom_bg = get_as_color BorderBottomBackgroundKey style in
  let left_bg = get_as_color BorderLeftBackgroundKey style in
  if
    border = Border.no_border
    || ((not has_top) && (not has_right) && (not has_bottom) && not has_left)
  then str
  else
    let lines, width = get_lines str in
    let border, width =
      if has_left then
        ( (if border.left = "" then {border with left= " "} else border)
        , width + max_rune_width border.left )
      else (border, width)
    in
    let border =
      if has_right && border.right = "" then {border with right= " "}
      else border
    in
    let border =
      if has_top && has_left && border.top_left = "" then
        {border with top_left= " "}
      else border
    in
    let border =
      if has_top && has_right && border.top_right = "" then
        {border with top_right= " "}
      else border
    in
    let border =
      if has_bottom && has_left && border.bottom_left = "" then
        {border with bottom_left= " "}
      else border
    in
    let border =
      if has_bottom && has_right && border.bottom_right = "" then
        {border with bottom_right= " "}
      else border
    in
    let border =
      if has_top then
        match (has_left, has_right) with
        | false, false ->
            {border with top_left= ""; top_right= ""}
        | false, true ->
            {border with top_left= ""}
        | true, false ->
            {border with top_right= ""}
        | true, true ->
            border
      else border
    in
    let border =
      if has_bottom then
        match (has_left, has_right) with
        | false, false ->
            {border with bottom_left= ""; bottom_right= ""}
        | false, true ->
            {border with bottom_left= ""}
        | true, false ->
            {border with bottom_right= ""}
        | true, true ->
            border
      else border
    in
    let border =
      { border with
        top_left= get_first_rune_as_string border.top_left
      ; top_right= get_first_rune_as_string border.top_right
      ; bottom_right= get_first_rune_as_string border.bottom_right
      ; bottom_left= get_first_rune_as_string border.bottom_left }
    in
    let buffer = Buffer.create 1024 in
    if has_top then (
      let top =
        render_horizontal_edge border.top_left border.top border.top_right width
      in
      let top = style_border style top top_fg top_bg in
      Buffer.add_string buffer top ;
      Buffer.add_char buffer '\n' ) ;
    let left_runes = runes border.left in
    let left_index = ref 0 in
    let right_runes = runes border.right in
    let right_index = ref 0 in
    let lines_lenght = List.length lines in
    List.iteri
      (fun i l ->
        if has_left then (
          let r = string left_runes.(!left_index) in
          incr left_index ;
          if !left_index >= Array.length left_runes then left_index := 0 ;
          Buffer.add_string buffer (style_border style r left_fg left_bg) ) ;
        Buffer.add_string buffer l ;
        if has_right then (
          let r = string right_runes.(!right_index) in
          incr right_index ;
          if !right_index >= Array.length right_runes then right_index := 0 ;
          Buffer.add_string buffer (style_border style r right_fg right_bg) ) ;
        if i < lines_lenght - 1 then Buffer.add_char buffer '\n' )
      lines ;
    if has_bottom then (
      let bottom =
        render_horizontal_edge border.bottom_left border.bottom
          border.bottom_right width
      in
      let bottom = style_border style bottom bottom_fg bottom_bg in
      Buffer.add_char buffer '\n' ;
      Buffer.add_string buffer bottom ) ;
    Buffer.contents buffer

(** JOIN **)

let join_horizontal pos = function
  | [] ->
      ""
  | [s] ->
      s
  | l ->
      let max_height, blocks =
        List.fold_left_map
          (fun height block ->
            let lines, width = get_lines block in
            let lines = Array.of_list lines in
            (max height (Array.length lines), (lines, width)) )
          0 l
      in
      let blocks =
        List.map
          (fun (lines, width) ->
            let extra_lines = Array.make (max_height - Array.length lines) "" in
            match Position.value pos with
            | 0. ->
                (Array.append lines extra_lines, width)
            | 1. ->
                (Array.append extra_lines lines, width)
            | _ ->
                assert false )
          blocks
      in
      let block_length = Array.length (fst (List.hd blocks)) in
      let buffer = Buffer.create 1024 in
      for i = 0 to block_length - 1 do
        List.iter
          (fun (block, width) ->
            Buffer.add_string buffer block.(i) ;
            Buffer.add_string buffer
              (String.make (width - Ansi.string_width block.(i)) ' ') )
          blocks ;
        if i < block_length - 1 then Buffer.add_char buffer '\n'
      done ;
      Buffer.contents buffer

let join_vertical pos = function
  | [] ->
      ""
  | [s] ->
      s
  | l ->
      let max_width, blocks =
        List.fold_left_map
          (fun maxWidth block ->
            let lines, width = get_lines block in
            (max maxWidth width, lines) )
          0 l
      in
      let blocks_len = List.length blocks in
      let buffer = Buffer.create 1024 in
      List.iteri
        (fun i block ->
          let block_len = List.length block in
          List.iteri
            (fun j line ->
              let w = max_width - Ansi.string_width line in
              ( match Position.value pos with
              | 0. ->
                  Buffer.add_string buffer line ;
                  Buffer.add_string buffer (String.make w ' ')
              | 1. ->
                  Buffer.add_string buffer (String.make w ' ') ;
                  Buffer.add_string buffer line
              | pos ->
                  if w < 1 then Buffer.add_string buffer line
                  else
                    let split =
                      Int.of_float (Float.round (Float.of_int w *. pos))
                    in
                    let right = w - split in
                    let left = w - right in
                    Buffer.add_string buffer (String.make left ' ') ;
                    Buffer.add_string buffer line ;
                    Buffer.add_string buffer (String.make right ' ') ) ;
              if not (i = blocks_len - 1 && j = block_len - 1) then
                Buffer.add_char buffer '\n' )
            block )
        blocks ;
      Buffer.contents buffer

(** ALIGN **)

let align_text_horizontal str pos width style =
  let lines, widest_line = get_lines str in
  let buffer = Buffer.create 4096 in
  let length = List.length lines in
  List.iteri
    (fun i line ->
      let line_width = Ansi.string_width line in
      let short_amount = widest_line - line_width in
      let short_amount =
        short_amount + max 0 (width - (short_amount + line_width))
      in
      if short_amount > 0 then (
        match Position.value pos with
        | 1. ->
            let s = String.make short_amount ' ' in
            let s =
              Option.fold ~none:s
                ~some:(fun style -> Ansi.Style.styled s style)
                style
            in
            Buffer.add_string buffer s ;
            Buffer.add_string buffer line
        | 0.5 ->
            let left = short_amount / 2 in
            let right = left + (short_amount mod 2) in
            let left_spaces = String.make left ' ' in
            let right_spaces = String.make right ' ' in
            let left_spaces, right_spaces =
              Option.fold
                ~none:(left_spaces, right_spaces)
                ~some:(fun style ->
                  ( Ansi.Style.styled left_spaces style
                  , Ansi.Style.styled right_spaces style ) )
                style
            in
            Buffer.add_string buffer left_spaces ;
            Buffer.add_string buffer line ;
            Buffer.add_string buffer right_spaces
        | _ ->
            let s = String.make short_amount ' ' in
            let s =
              Option.fold ~none:s
                ~some:(fun style -> Ansi.Style.styled s style)
                style
            in
            Buffer.add_string buffer line ;
            Buffer.add_string buffer s )
      else Buffer.add_string buffer line ;
      if i <> length - 1 then Buffer.add_char buffer '\n' )
    lines ;
  Buffer.contents buffer

module Whitespace = struct
  type t = {chars: string; style: style}

  let create () = {chars= ""; style= new_style ()}

  let render width renderer t =
    let chars = if t.chars = "" then " " else t.chars in
    let r = runes chars in
    let i = ref 0 in
    let j = ref 0 in
    let buffer = Buffer.create 512 in
    while !i < width do
      Buffer.add_utf_8_uchar buffer r.(!j) ;
      incr j ;
      if !j >= Array.length r then j := 0 ;
      i := !i + Ansi.string_width (string r.(!j))
    done ;
    let short = width - Ansi.string_width (Buffer.contents buffer) in
    if short > 0 then Buffer.add_string buffer (String.make short ' ') ;
    renderer [Buffer.contents buffer] t.style

  let chars chars t = {t with chars}

  let style style t = {t with style}
end

(** POSITION **)

let place_horizontal width pos str whitespace renderer =
  let lines, content_width = get_lines str in
  let len_lines = List.length lines in
  let gap = width - content_width in
  if gap <= 0 then str
  else
    let buffer = Buffer.create 4096 in
    List.iteri
      (fun i l ->
        let short = max 0 (content_width - Ansi.string_width l) in
        ( match Position.value pos with
        | 0. ->
            Buffer.add_string buffer l ;
            Buffer.add_string buffer
              (Whitespace.render (gap + short) renderer whitespace)
        | 1. ->
            Buffer.add_string buffer
              (Whitespace.render (gap + short) renderer whitespace) ;
            Buffer.add_string buffer l
        | f ->
            let total_gap = gap + short in
            let split =
              Int.of_float (Float.round (Float.of_int total_gap *. f))
            in
            let left = total_gap - split in
            let right = total_gap - left in
            Buffer.add_string buffer
              (Whitespace.render left renderer whitespace) ;
            Buffer.add_string buffer l ;
            Buffer.add_string buffer
              (Whitespace.render right renderer whitespace) ) ;
        if i < len_lines - 1 then Buffer.add_char buffer '\n' )
      lines ;
    Buffer.contents buffer

let place_vertical height pos str whitespace renderer =
  let content_height =
    String.fold_left (fun acc c -> if c = '\n' then acc + 1 else acc) 1 str
  in
  let gap = height - content_height in
  if gap <= 0 then str
  else
    let _, width = get_lines str in
    let empty_line = Whitespace.render width renderer whitespace in
    let buffer = Buffer.create 4096 in
    ( match Position.value pos with
    | 0. ->
        Buffer.add_string buffer str ;
        for _ = 0 to gap - 1 do
          Buffer.add_char buffer '\n' ;
          Buffer.add_string buffer empty_line
        done
    | 1. ->
        for _ = 0 to gap - 1 do
          Buffer.add_string buffer empty_line ;
          Buffer.add_char buffer '\n'
        done ;
        Buffer.add_string buffer str
    | f ->
        let split = Int.of_float (Float.round (Float.of_int gap *. f)) in
        let top = gap - split in
        let bottom = gap - top in
        for _ = 0 to top - 1 do
          Buffer.add_string buffer empty_line ;
          Buffer.add_char buffer '\n'
        done ;
        Buffer.add_string buffer str ;
        for _ = 0 to bottom - 1 do
          Buffer.add_char buffer '\n' ;
          Buffer.add_string buffer empty_line
        done ) ;
    Buffer.contents buffer

let place width height h_pos v_pos str whitespace renderer =
  place_vertical height v_pos
    (place_horizontal width h_pos str whitespace renderer)
    whitespace renderer

(** STYLE **)

let pad str n (style : Ansi.Style.style option) =
  if n = 0 then str
  else
    let sp = String.make (abs n) ' ' in
    let sp =
      Option.fold ~none:sp ~some:(fun style -> Ansi.Style.styled sp style) style
    in
    let b = Buffer.create 4096 in
    let l = String.split_on_char '\n' str in
    let length = List.length l in
    List.iteri
      (fun i line ->
        if n > 0 then (Buffer.add_string b line ; Buffer.add_string b sp)
        else (Buffer.add_string b sp ; Buffer.add_string b line) ;
        if i <> length - 1 then Buffer.add_char b '\n' )
      l ;
    Buffer.contents b

let pad_left str n style = pad str (-n) style

let pad_right str n style = pad str n style

let apply_margins style str =
  let top_margin = get_as_int MarginTopKey style in
  let right_margin = get_as_int MarginRightKey style in
  let bottom_margin = get_as_int MarginBottomKey style in
  let left_margin = get_as_int MarginLeftKey style in
  let bgc = get_as_color MarginBackgroundKey style in
  let style = Ansi.Style.create () in
  let style =
    if bgc <> NoColor then Ansi.Style.background_color bgc style else style
  in
  let str = pad_left str left_margin (Some style) in
  let str = pad_right str right_margin (Some style) in
  let _, width = get_lines str in
  let spaces = String.make width ' ' in
  let buffer = Buffer.create 4096 in
  let rec iter ~f = function
    | 0 ->
        ()
    | n ->
        f () ;
        iter ~f (n - 1)
  in
  if top_margin > 0 then (
    let top_buffer = Buffer.create 1024 in
    iter
      ~f:(fun () ->
        Buffer.add_string top_buffer spaces ;
        Buffer.add_char top_buffer '\n' )
      top_margin ;
    let top_str = Ansi.Style.styled (Buffer.contents top_buffer) style in
    Buffer.add_string buffer top_str ) ;
  Buffer.add_string buffer str ;
  if bottom_margin > 0 then (
    let bottom_buffer = Buffer.create 1024 in
    iter
      ~f:(fun () ->
        Buffer.add_char bottom_buffer '\n' ;
        Buffer.add_string bottom_buffer spaces )
      top_margin ;
    let top_str = Ansi.Style.styled (Buffer.contents bottom_buffer) style in
    Buffer.add_string buffer top_str ) ;
  Buffer.contents buffer

let render strs style =
  let strs = if style.value <> "" then style.value :: strs else strs in
  let str = String.concat "" strs in
  let te = Ansi.Style.create () in
  let te_whitespace = Ansi.Style.create () in
  let _te_space = Ansi.Style.create () in
  let bold = get_as_bool BoldKey false style in
  let italic = get_as_bool ItalicKey false style in
  let underline = get_as_bool UnderlineKey false style in
  let strikethrough = get_as_bool StrikethroughKey false style in
  let reverse = get_as_bool ReverseKey false style in
  let blink = get_as_bool BlinkKey false style in
  let faint = get_as_bool FaintKey false style in
  let fg = get_as_color ForegroundKey style in
  let bg = get_as_color BackgroundKey style in
  let width = get_as_int WidthKey style in
  let _height = get_as_int HeightKey style in
  let horizontal_align = get_as_position AlignHorizontalKey style in
  let top_padding = get_as_int PaddingTopKey style in
  let right_padding = get_as_int PaddingRightKey style in
  let bottom_padding = get_as_int PaddingBottomKey style in
  let left_padding = get_as_int PaddingLeftKey style in
  let color_whitespace = get_as_bool ColorWhitespaceKey true style in
  let te = if bold then Ansi.Style.bold te else te in
  let te = if italic then Ansi.Style.italic te else te in
  let te = if underline then Ansi.Style.underline te else te in
  let te, te_whitespace =
    if reverse then (Ansi.Style.reverse te, Ansi.Style.reverse te_whitespace)
    else (te, te_whitespace)
  in
  let te = if blink then Ansi.Style.slow_blink te else te in
  let te = if faint then Ansi.Style.faint te else te in
  let te = if fg <> NoColor then Ansi.Style.foreground_color fg te else te in
  let te, te_whitespace =
    if bg <> NoColor then
      ( Ansi.Style.background_color bg te
      , if color_whitespace then Ansi.Style.background_color bg te_whitespace
        else te_whitespace )
    else (te, te_whitespace)
  in
  let te = if underline then Ansi.Style.underline te else te in
  let te = if strikethrough then Ansi.Style.strikethrough te else te in
  let str =
    let buffer = Buffer.create 4096 in
    let l = String.split_on_char '\n' str in
    let l_len = List.length l in
    List.iteri
      (fun i line ->
        Buffer.add_string buffer (Ansi.Style.styled line te) ;
        if i <> l_len - 1 then Buffer.add_char buffer '\n' )
      l ;
    Buffer.contents buffer
  in
  let str =
    if left_padding > 0 then
      let st = if color_whitespace then Some te_whitespace else None in
      pad_left str left_padding st
    else str
  in
  let str =
    if right_padding > 0 then
      let st = if color_whitespace then Some te_whitespace else None in
      pad_right str right_padding st
    else str
  in
  let str =
    if top_padding > 0 then String.make top_padding '\n' ^ str else str
  in
  let str =
    if bottom_padding > 0 then str ^ String.make bottom_padding '\n' else str
  in
  let num_lines =
    String.fold_left (fun acc c -> if c = '\n' then acc + 1 else acc) 0 str
  in
  let str =
    if not (num_lines = 0 && width = 0) then
      let st = if color_whitespace then Some te_whitespace else None in
      align_text_horizontal str horizontal_align width st
    else str
  in
  let str = apply_border style str in
  let str = apply_margins style str in
  str

let string style = render [] style
