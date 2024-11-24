module Profile = struct
  type t = TrueColor | ANSI256 | ANSI | ASCII | NoTTY

  let string = function
    | TrueColor ->
        "TrueColor"
    | ANSI256 ->
        "ANSI256"
    | ANSI ->
        "ANSI"
    | ASCII ->
        "ASCII"
    | NoTTY ->
        "NoTTY"
end

module Env = struct
  let color_term env =
    let color_term = String.lowercase_ascii (env "COLORTERM") in
    color_term = "truecolor" || color_term = "24bit" || color_term = "yes"
    || color_term = "true"

  let rec string_contains ~sub = function
    | "" ->
        sub = "" (* the empty string contains itself *)
    | s ->
        String.starts_with ~prefix:sub s
        || string_contains ~sub (String.sub s 1 (String.length s - 1))

  let parse_bool string =
    match String.lowercase_ascii string with "true" | "1" -> true | _ -> false

  let env_no_color env = parse_bool (env "NO_COLOR")

  let cli_color env = parse_bool (env "CLICOLOR")

  let cli_color_forced env = parse_bool (env "CLICOLOR_FORCE")

  let env_color_profile environ =
    let term = environ "TERM" in
    let term = String.lowercase_ascii term in
    if
      color_term environ
      && (not (String.starts_with ~prefix:"screen" term))
      && not (String.starts_with ~prefix:"tmux" term)
    then Profile.TrueColor
    else
      match term with
      | "" | "dumb" ->
          Profile.NoTTY
      | "alacritty" | "contour" | "wezterm" | "xterm-ghostty" | "xterm-kitty" ->
          TrueColor
      | "linux" ->
          ANSI
      | "screen" ->
          ANSI256
      | _ ->
          if string_contains ~sub:"256color" term then ANSI256
          else if string_contains ~sub:"color" term then ANSI
          else if string_contains ~sub:"ansi" term then ANSI
          else ASCII

  let color_profile isatty environ =
    let env_profile = env_color_profile environ in
    let term = String.lowercase_ascii (environ "TERM") in
    let is_dumb = term = "dumb" in
    let p = if (not isatty) && is_dumb then Profile.NoTTY else env_profile in
    if env_no_color environ then if p < ASCII then Profile.ASCII else p
    else if cli_color_forced environ then
      let p = if p > ANSI then Profile.ANSI else p in
      if env_profile < p then env_profile else p
    else if cli_color environ && isatty && (not is_dumb) && p > ANSI then ANSI
    else p

  let detect _output environ =
    let isatty = true in
    color_profile isatty environ

  let env environ = color_profile true environ
end

module Writer = struct
  type t = {forward: Format.formatter; profile: Profile.t}

  let create forward environ = {forward; profile= Env.detect forward environ}
end
