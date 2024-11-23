module Parser = struct
  (** CONST **)

  type action =
    | NoneAction
    | ClearAction
    | CollectAction
    | MarkerAction
    | DispatchAction
    | ExecuteAction
    | StartAction
    | PutAction
    | ParamAction
    | PrintAction
    | IgnoreAction

  let action_to_value = function
    | NoneAction | IgnoreAction ->
        0
    | ClearAction ->
        1
    | CollectAction ->
        2
    | MarkerAction ->
        3
    | DispatchAction ->
        4
    | ExecuteAction ->
        5
    | StartAction ->
        6
    | PutAction ->
        7
    | ParamAction ->
        8
    | PrintAction ->
        9

  let action_of_value = function
    | 0 ->
        NoneAction
    | 1 ->
        ClearAction
    | 2 ->
        CollectAction
    | 3 ->
        MarkerAction
    | 4 ->
        DispatchAction
    | 5 ->
        ExecuteAction
    | 6 ->
        StartAction
    | 7 ->
        PutAction
    | 8 ->
        ParamAction
    | 9 ->
        PrintAction
    | _ ->
        IgnoreAction

  type state =
    | GroundState
    | CSIEntryState
    | CSIIntermediateState
    | CSIParamState
    | DCSEntryState
    | DCSIntermediateState
    | DCSParamState
    | DCSStringState
    | EscapeState
    | EscapeIntermediateState
    | OSCStringState
    | SOSStringState
    | PMStringState
    | APCStringState
    (* UTF8State is not part of the DEC ANSI standard. It is used to handle UTF-8 sequences. *)
    | UTF8State

  let state_to_value = function
    | GroundState ->
        0
    | CSIEntryState ->
        1
    | CSIIntermediateState ->
        2
    | CSIParamState ->
        3
    | DCSEntryState ->
        4
    | DCSIntermediateState ->
        5
    | DCSParamState ->
        6
    | DCSStringState ->
        7
    | EscapeState ->
        8
    | EscapeIntermediateState ->
        9
    | OSCStringState ->
        10
    | SOSStringState ->
        11
    | PMStringState ->
        12
    | APCStringState ->
        13
    | UTF8State ->
        14

  let state_of_value = function
    | 0 ->
        GroundState
    | 1 ->
        CSIEntryState
    | 2 ->
        CSIIntermediateState
    | 3 ->
        CSIParamState
    | 4 ->
        DCSEntryState
    | 5 ->
        DCSIntermediateState
    | 6 ->
        DCSParamState
    | 7 ->
        DCSEntryState
    | 8 ->
        EscapeState
    | 9 ->
        EscapeIntermediateState
    | 10 ->
        OSCStringState
    | 11 ->
        SOSStringState
    | 12 ->
        PMStringState
    | 13 ->
        APCStringState
    | _ ->
        UTF8State

  (** TRANSITION TABLE **)

  let transition_action_shift = 4

  let transition_state_mask = 15

  let index_state_shift = 8

  let default_table_size = 4096

  type transition_table = int Array.t

  let new_transition_table size =
    let size = if size <= 0 then default_table_size else size in
    Array.make size 0

  let set_default table action state =
    let action = action_to_value action in
    let state = state_to_value state in
    for i = 0 to Array.length table - 1 do
      Array.set table i ((action lsl transition_action_shift) lor state)
    done

  let add_one table code state action next =
    let state = state_to_value state in
    let action = action_to_value action in
    let next = state_to_value next in
    let idx = (state lsl index_state_shift) lor code in
    let value = (action lsl transition_action_shift) lor next in
    Array.set table idx value

  let add_many table codes state action next =
    List.iter (fun code -> add_one table code state action next) codes

  let add_range table start_byte end_byte state action next =
    for i = start_byte to end_byte do
      add_one table i state action next
    done

  let transition table state code =
    let state = state_to_value state in
    let index = (state lsl index_state_shift) lor code in
    let value = Array.get table index in
    ( state_of_value (value land transition_state_mask)
    , action_of_value (value lsr transition_action_shift) )

  let r start_state end_state =
    let rec aux start_byte end_byte =
      if start_byte <= end_byte then
        state_of_value start_byte :: aux (start_byte + 1) end_byte
      else []
    in
    aux (state_to_value start_state) (state_to_value end_state)

  let generate_transition_table () =
    let table = new_transition_table default_table_size in
    set_default table NoneAction GroundState ;
    List.iter
      (fun state ->
        (* Anywhere -> Ground *)
        add_many table [0x18; 0x1A; 0x99; 0x9A] state ExecuteAction GroundState ;
        add_range table 0x80 0x8F state ExecuteAction GroundState ;
        add_range table 0x90 0x97 state ExecuteAction GroundState ;
        add_one table 0x9C state ExecuteAction GroundState ;
        (* Anywhere -> Escape *)
        add_one table 0x1B state ClearAction EscapeState ;
        (* Anywhere -> SOSStringState *)
        add_one table 0x98 state StartAction SOSStringState ;
        (* Anywhere -> PMStringState *)
        add_one table 0x9E state StartAction PMStringState ;
        (* Anywhere -> APCStringState *)
        add_one table 0x9F state StartAction APCStringState ;
        (* Anywhere -> CSIEntryState *)
        add_one table 0x9B state ClearAction CSIEntryState ;
        (* Anywhere -> DCSEntryState *)
        add_one table 0x90 state ClearAction DCSEntryState ;
        (* Anywhere -> OCSStringState *)
        add_one table 0x9D state StartAction OSCStringState ;
        (* Anywhere -> UTF8State *)
        add_range table 0xC2 0xDF state CollectAction UTF8State ;
        add_range table 0xE0 0xEF state CollectAction UTF8State ;
        add_range table 0xF0 0xF4 state CollectAction UTF8State )
      (r GroundState UTF8State) ;
    (* Ground *)
    add_range table 0x00 0x17 GroundState ExecuteAction GroundState ;
    add_one table 0x19 GroundState ExecuteAction GroundState ;
    add_range table 0x1C 0x1F GroundState ExecuteAction GroundState ;
    add_range table 0x20 0x7E GroundState PrintAction GroundState ;
    add_one table 0x7F GroundState ExecuteAction GroundState ;
    (* EscapeIntermediate *)
    add_range table 0x00 0x17 EscapeIntermediateState ExecuteAction
      EscapeIntermediateState ;
    add_one table 0x19 EscapeIntermediateState ExecuteAction
      EscapeIntermediateState ;
    add_range table 0x1C 0x1F EscapeIntermediateState ExecuteAction
      EscapeIntermediateState ;
    add_range table 0x20 0x2F EscapeIntermediateState CollectAction
      EscapeIntermediateState ;
    add_one table 0x7F EscapeIntermediateState IgnoreAction
      EscapeIntermediateState ;
    (* EscapeIntermediate -> Ground *)
    add_range table 0x30 0x7E EscapeIntermediateState DispatchAction GroundState ;
    (* Escape *)
    add_range table 0x00 0x17 EscapeState ExecuteAction EscapeState ;
    add_one table 0x19 EscapeState ExecuteAction EscapeState ;
    add_range table 0x1C 0x1F EscapeState ExecuteAction EscapeState ;
    add_one table 0x7F EscapeState IgnoreAction EscapeState ;
    (* Escape -> Ground *)
    add_range table 0x30 0x3F EscapeState DispatchAction GroundState ;
    add_range table 0x51 0x57 EscapeState DispatchAction GroundState ;
    add_one table 0x59 EscapeState DispatchAction GroundState ;
    add_one table 0x5A EscapeState DispatchAction GroundState ;
    add_one table 0x5C EscapeState DispatchAction GroundState ;
    add_range table 0x60 0x7E EscapeState DispatchAction GroundState ;
    (* Escape -> EscapeIntermediate *)
    add_range table 0x20 0x2F EscapeState CollectAction EscapeIntermediateState ;
    (* Escape -> SOSPMAPCString *)
    add_one table (Char.code 'X') EscapeState StartAction SOSStringState ;
    add_one table (Char.code '^') EscapeState StartAction PMStringState ;
    add_one table (Char.code '_') EscapeState StartAction APCStringState ;
    (* Escape -> DCSEntry *)
    add_one table (Char.code 'P') EscapeState ClearAction DCSEntryState ;
    (* Escape -> CSIEntry *)
    add_one table (Char.code '[') EscapeState ClearAction CSIEntryState ;
    (* Escape -> OSCString *)
    add_one table (Char.code ']') EscapeState StartAction OSCStringState ;
    (* SOSPMAPCString *)
    List.iter
      (fun state ->
        add_range table 0x00 0x17 state PutAction state ;
        add_one table 0x19 state PutAction state ;
        add_range table 0x1C 0x1F state PutAction state ;
        add_range table 0x20 0x7F state PutAction state ;
        (* ESC, ST, CAN and SUB terminate the sequence *)
        add_one table 0x1B state DispatchAction EscapeState ;
        add_one table 0x9C state DispatchAction GroundState ;
        add_many table [0x18; 0x1A] state IgnoreAction GroundState )
      (r SOSStringState APCStringState) ;
    (* DCSEntry *)
    add_range table 0x00 0x07 DCSEntryState IgnoreAction DCSEntryState ;
    add_range table 0x0E 0x17 DCSEntryState IgnoreAction DCSEntryState ;
    add_one table 0x19 DCSEntryState IgnoreAction DCSEntryState ;
    add_range table 0x1C 0x1F DCSEntryState IgnoreAction DCSEntryState ;
    add_one table 0x7F DCSEntryState IgnoreAction DCSEntryState ;
    (* DCSEntry -> DCSIntermediate *)
    add_range table 0x20 0x2F DCSEntryState CollectAction DCSIntermediateState ;
    (* DCSEntry -> DCSParam *)
    add_range table 0x30 0x3B DCSEntryState ParamAction DCSParamState ;
    add_range table 0x3C 0x3F DCSEntryState MarkerAction DCSParamState ;
    (* DCSEntry -> DCSPasstrough *)
    add_range table 0x08 0x0D DCSEntryState PutAction DCSStringState ;
    add_one table 0x1B DCSEntryState PutAction DCSStringState ;
    add_range table 0x40 0x7E DCSEntryState StartAction DCSStringState ;
    (* DCSIntermediate *)
    add_range table 0x00 0x17 DCSIntermediateState IgnoreAction
      DCSIntermediateState ;
    add_one table 0x19 DCSIntermediateState IgnoreAction DCSIntermediateState ;
    add_range table 0x1C 0x1F DCSIntermediateState IgnoreAction
      DCSIntermediateState ;
    add_range table 0x20 0x2F DCSIntermediateState CollectAction
      DCSIntermediateState ;
    add_one table 0x7F DCSIntermediateState IgnoreAction DCSIntermediateState ;
    (* DCSIntermediate -> DCSPasstrough *)
    add_range table 0x30 0x3F DCSIntermediateState StartAction DCSStringState ;
    add_range table 0x40 0x7E DCSIntermediateState StartAction DCSStringState ;
    (* DCSParam *)
    add_range table 0x00 0x17 DCSParamState IgnoreAction DCSParamState ;
    add_one table 0x19 DCSParamState IgnoreAction DCSParamState ;
    add_range table 0x1C 0x1F DCSParamState IgnoreAction DCSParamState ;
    add_range table 0x30 0x3B DCSParamState ParamAction DCSParamState ;
    add_one table 0x7F DCSParamState IgnoreAction DCSParamState ;
    add_range table 0x3C 0x3F DCSParamState IgnoreAction DCSParamState ;
    (* DCSParam -> DCSIntermediate *)
    add_range table 0x20 0x2F DCSParamState CollectAction DCSIntermediateState ;
    (* DCSParam -> DCSPasstrough *)
    add_range table 0x40 0x7E DCSParamState StartAction DCSStringState ;
    (* DCSPasstrought *)
    add_range table 0x00 0x17 DCSStringState PutAction DCSStringState ;
    add_one table 0x19 DCSStringState PutAction DCSStringState ;
    add_range table 0x1C 0x1F DCSStringState PutAction DCSStringState ;
    add_range table 0x20 0x7E DCSStringState PutAction DCSStringState ;
    add_one table 0x7F DCSStringState PutAction DCSStringState ;
    add_range table 0x80 0xFF DCSStringState PutAction DCSStringState ;
    (* ST, CAN, SUB, and ESC terminate the sequence *)
    add_one table 0x1B DCSStringState DispatchAction EscapeState ;
    add_one table 0x9C DCSStringState DispatchAction GroundState ;
    add_many table [0x18; 0x1A] DCSStringState IgnoreAction GroundState ;
    (* CSIParam *)
    add_range table 0x00 0x17 CSIParamState ExecuteAction CSIParamState ;
    add_one table 0x19 CSIParamState ExecuteAction CSIParamState ;
    add_range table 0x1C 0x1F CSIParamState ExecuteAction CSIParamState ;
    add_range table 0x30 0x3B CSIParamState ParamAction CSIParamState ;
    add_one table 0x7F CSIParamState IgnoreAction CSIParamState ;
    add_range table 0x3C 0x3F CSIParamState IgnoreAction CSIParamState ;
    (* CSIParam -> Ground *)
    add_range table 0x40 0x7E CSIParamState DispatchAction GroundState ;
    (* CSIParam -> CSIIntermediate *)
    add_range table 0x20 0x2F CSIParamState CollectAction CSIIntermediateState ;
    (* CSIIntermediate *)
    add_range table 0x00 0x17 CSIIntermediateState ExecuteAction
      CSIIntermediateState ;
    add_one table 0x19 CSIIntermediateState ExecuteAction CSIIntermediateState ;
    add_range table 0x1C 0x1F CSIIntermediateState ExecuteAction
      CSIIntermediateState ;
    add_range table 0x20 0x2F CSIIntermediateState CollectAction
      CSIIntermediateState ;
    add_one table 0x7F CSIIntermediateState IgnoreAction CSIIntermediateState ;
    (* CSIIntermediate -> Ground *)
    add_range table 0x40 0x7E CSIIntermediateState DispatchAction GroundState ;
    (* CSIIntermediate -> CSIIgnore *)
    add_range table 0x30 0x3F CSIIntermediateState IgnoreAction GroundState ;
    (* CSIEntry *)
    add_range table 0x00 0x17 CSIEntryState ExecuteAction CSIEntryState ;
    add_one table 0x19 CSIEntryState ExecuteAction CSIEntryState ;
    add_range table 0x1C 0x1F CSIEntryState ExecuteAction CSIEntryState ;
    add_one table 0x7F CSIEntryState IgnoreAction CSIEntryState ;
    (* CSIEntry -> Ground *)
    add_range table 0x40 0x7E CSIEntryState DispatchAction GroundState ;
    (* CSIEntry -> CSIIntermediate *)
    add_range table 0x20 0x2F CSIEntryState CollectAction CSIIntermediateState ;
    (* CSIEntry -> CSIParma *)
    add_range table 0x30 0x3B CSIEntryState ParamAction CSIParamState ;
    add_range table 0x3C 0x3F CSIEntryState MarkerAction CSIParamState ;
    (* OSCString *)
    add_range table 0x00 0x06 OSCStringState IgnoreAction OSCStringState ;
    add_range table 0x08 0x17 OSCStringState IgnoreAction OSCStringState ;
    add_one table 0x19 OSCStringState IgnoreAction OSCStringState ;
    add_range table 0x1C 0x1F OSCStringState IgnoreAction OSCStringState ;
    add_range table 0x20 0xFF OSCStringState PutAction OSCStringState ;
    (* ST, CAN, SUB, ESC, and BEL terminate the sequence *)
    add_one table 0x1B OSCStringState DispatchAction EscapeState ;
    add_one table 0x07 OSCStringState DispatchAction GroundState ;
    add_one table 0x9C OSCStringState DispatchAction GroundState ;
    add_many table [0x18; 0x1A] OSCStringState IgnoreAction GroundState ;
    table

  let table = generate_transition_table ()
end

(** WIDTH **)

let string_width str =
  if str = "" then 0
  else
    let pstate = ref Parser.GroundState in
    let width = ref 0 in
    let i = ref 0 in
    while !i < String.length str do
      let state, action =
        Parser.transition Parser.table !pstate (Char.code (String.get str !i))
      in
      if state = Parser.UTF8State then (
        let cluster = String.get_utf_8_uchar str !i in
        let cluster = Uchar.utf_decode_uchar cluster in
        pstate := GroundState ;
        width := !width + Uucp.Break.tty_width_hint cluster ;
        i := !i + Uchar.utf_8_byte_length cluster - 1 )
      else (
        if action = Parser.PrintAction then incr width ;
        pstate := state ) ;
      incr i
    done ;
    !width

(*let string_width str =
  Uutf.String.fold_utf_8
    (fun x _ -> function
      | `Malformed _ ->
          failwith "max_run_width"
      | `Uchar uc ->
          x + Uucp.Break.tty_width_hint uc )
    0 str*)
