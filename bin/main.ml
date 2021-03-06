open Lwt
open LTerm_geom
open LTerm_key
open Koiiword
open Koiiword.Board
open Koiiword.State
open Koiiword.Layout
open Koiiword.Generate_letters
open Koiiword.Player
open Koiiword.Points
open Koiiword.Entry
open Koiiword.Util
open Koiiword.Dictionary
open CamomileLibrary

let ( let* ) = Lwt.bind

let global_dictionary : dictionary Lazy.t =
  Lazy.from_fun (fun _ -> dict_from_file "dictionary/english_dict.txt")

(** [sort_players players] is a list of players types sorted
    lexicographically by name. *)
let sort_players players =
  let comp p1 p2 = Stdlib.compare p1.name p2.name in
  List.sort comp players

(** The [loop_result] type describes the response of the gameplay loop
    to an event. Currently, the game can respond by doing nothing
    ([LoopResultContinue]), updating the gameplay state and re-rendering
    ([LoopResultUpdateState]), or exiting ([LoopResultExit]) *)
type loop_result =
  | LoopResultContinue
  | LoopResultUpdateState of game_state
  | LoopResultExit

(* [get_point_delta state] is the number of points that should be added
   to the current player's points field given the most recently inputted
   word in state.*)
let get_point_delta
    (prev_state : gameplay_state)
    (current_state : gameplay_state) =
  let prev_words = get_words_deep prev_state.board in
  let current_words = get_words_deep current_state.board in
  List.fold_left
    (fun score word ->
      if List.mem word prev_words then score
      else score + word_points word)
    0 current_words

(* [with_updated_player_score prev_state current_state]returns a
   gameplay state where the current player's score has been increased by
   the value of the words that were added between [prev_state] and
   [current_state] *)
let with_updated_player_score
    (prev_state : gameplay_state)
    (current_state : gameplay_state) : gameplay_state =
  let current_player =
    List.nth current_state.players current_state.current_player_index
  in
  let players =
    Util.set current_state.players current_state.current_player_index
      {
        current_player with
        points =
          current_player.points
          + get_point_delta prev_state current_state;
        letters = refill_deck current_player.letters;
      }
  in
  { current_state with players }

let with_input_direction
    (start : position)
    (direction : direction)
    (current_state : gameplay_state) =
  let { players; current_player_index; _ } = current_state in
  let current_player = List.nth players current_player_index in
  let current_deck = current_player.letters in
  {
    current_state with
    entry =
      AddLetter { start; direction; deck = current_deck; word = [] };
  }

let with_deck (new_deck : letter_deck) (current_state : gameplay_state)
    =
  let { players; current_player_index; _ } = current_state in
  let current_player = List.nth players current_player_index in
  {
    current_state with
    players =
      Util.set players current_player_index
        { current_player with letters = new_deck };
  }

let with_pan (delta_pan : position) (current_state : gameplay_state) =
  {
    current_state with
    board =
      {
        current_state.board with
        pan =
          ( fst current_state.board.pan + fst delta_pan,
            snd current_state.board.pan + snd delta_pan );
      };
  }

let with_cursor
    (delta_cursor : position)
    (current_state : gameplay_state) =
  let pan_threshold = 3 in
  let new_state =
    {
      current_state with
      board =
        {
          current_state.board with
          cursor =
            ( fst current_state.board.cursor + fst delta_cursor,
              snd current_state.board.cursor + snd delta_cursor );
        };
    }
  in
  (* if the cursor is more than [pan_threshold] units away from the
     panning center, move the panning center to catch up *)
  let row_delta =
    fst new_state.board.cursor - fst new_state.board.pan
  in
  let row_delta =
    max 0 (abs row_delta - pan_threshold) * Util.sign row_delta
  in
  let col_delta =
    snd new_state.board.cursor - snd new_state.board.pan
  in
  let col_delta =
    max 0 (abs col_delta - pan_threshold) * Util.sign col_delta
  in
  with_pan (row_delta, col_delta) new_state

let loop_gameplay (evt : LTerm_event.t) (current_state : gameplay_state)
    : loop_result =
  let { board; players; entry; current_player_index; dict; _ } =
    current_state
  in
  let { cursor; _ } = board in
  match evt with
  | LTerm_event.Key { code = Tab; _ } -> (
      match entry with
      | SelectStart ->
          let pan_back =
            {
              current_state with
              board =
                {
                  current_state.board with
                  cursor = (0, 0);
                  pan = (0, 0);
                };
            }
          in
          LoopResultUpdateState (Gameplay pan_back)
      | _ -> LoopResultContinue)
  | LTerm_event.Key { code = Up; _ } -> (
      match entry with
      | SelectStart ->
          LoopResultUpdateState
            (Gameplay (with_cursor (-1, 0) current_state))
      | SelectDirection { start } ->
          LoopResultUpdateState
            (Gameplay (with_input_direction start Up current_state))
      | _ -> LoopResultContinue)
  | LTerm_event.Key { code = Down; _ } -> (
      match entry with
      | SelectStart ->
          LoopResultUpdateState
            (Gameplay (with_cursor (1, 0) current_state))
      | SelectDirection { start } ->
          LoopResultUpdateState
            (Gameplay (with_input_direction start Down current_state))
      | _ -> LoopResultContinue)
  | LTerm_event.Key { code = Left; _ } -> (
      match entry with
      | SelectStart ->
          LoopResultUpdateState
            (Gameplay (with_cursor (0, -1) current_state))
      | SelectDirection { start } ->
          LoopResultUpdateState
            (Gameplay (with_input_direction start Left current_state))
      | _ -> LoopResultContinue)
  | LTerm_event.Key { code = Right; _ } -> (
      match entry with
      | SelectStart ->
          LoopResultUpdateState
            (Gameplay (with_cursor (0, 1) current_state))
      | SelectDirection { start } ->
          LoopResultUpdateState
            (Gameplay (with_input_direction start Right current_state))
      | _ -> LoopResultContinue)
  | LTerm_event.Key { code = Enter; _ } -> (
      match entry with
      | SelectStart ->
          (* if this is the first tile the user is placing, they have to
             place it at (0, 0) *)
          if (not (Hashtbl.length board.tiles = 0)) || cursor = (0, 0)
          then
            LoopResultUpdateState
              (Gameplay
                 {
                   current_state with
                   entry = SelectDirection { start = cursor };
                   instructions = Instructions.EntrySelectDirection;
                 })
          else LoopResultContinue
      | AddLetter { start; direction; word; deck; _ } -> (
          if List.length word < 1 then LoopResultContinue
          else
            let new_tiles = Hashtbl.copy board.tiles in
            let new_tiles =
              apply_entry_tiles new_tiles start direction word
            in

            try
              let new_words =
                get_words_deep { board with tiles = new_tiles }
              in

              (* get words from char list *)
              if List.for_all (is_word_valid dict) new_words then
                (* if all words are valid then accept it *)
                (* calculate the new state, then update player point totals *)
                let new_state =
                  {
                    current_state with
                    entry = SelectStart;
                    instructions = Instructions.EntrySelectStart;
                    board = { board with tiles = new_tiles };
                  }
                in
                let new_state =
                  with_updated_player_score current_state new_state
                in
                let current_player =
                  List.nth new_state.players
                    new_state.current_player_index
                in
                if current_player.points >= 100 then
                  LoopResultUpdateState
                    (Victory
                       {
                         winner = current_player;
                         players = new_state.players;
                       })
                else
                  LoopResultUpdateState
                    (Gameplay
                       {
                         new_state with
                         current_player_index =
                           (current_player_index + 1)
                           mod List.length players;
                       })
              else
                (* if any word is invalid, return their original deck
                   and put them back in reselect state *)
                LoopResultUpdateState
                  (Gameplay
                     {
                       (with_deck deck current_state) with
                       instructions = Instructions.EntrySelectStart;
                       entry = SelectStart;
                     })
            with Disconnected ->
              (* if the word was entered without being connected to (0,
                 0), then reset *)
              LoopResultUpdateState
                (Gameplay
                   {
                     (with_deck deck current_state) with
                     instructions = Instructions.EntrySelectStart;
                     entry = SelectStart;
                   }))
      | _ -> LoopResultContinue)
  | LTerm_event.Key { code = Escape; _ } -> (
      match entry with
      (* if they were in the middle of spelling a word, restore the
         original deck *)
      | AddLetter { deck; _ } ->
          LoopResultUpdateState
            (Gameplay
               {
                 (with_deck deck current_state) with
                 instructions = Instructions.EntrySelectStart;
                 entry = SelectStart;
               })
      (* otherwise, just switch back to select start mode *)
      | _ ->
          LoopResultUpdateState
            (Gameplay
               {
                 current_state with
                 entry = SelectStart;
                 instructions = Instructions.EntrySelectStart;
               }))
  | LTerm_event.Key { code = F1; _ } -> (
      match entry with
      | _ ->
          LoopResultUpdateState
            (Gameplay
               {
                 current_state with
                 current_player_index =
                   (current_player_index + 1) mod List.length players;
                 entry = SelectStart;
                 instructions = Instructions.EntrySelectStart;
               }))
  | LTerm_event.Key { code = LTerm_key.Char c; control = true; _ } -> (
      match UChar.char_of c with
      | 'c' -> LoopResultExit
      | _ -> LoopResultContinue)
  | LTerm_event.Key { code = LTerm_key.Char c; control = false; _ } -> (
      match entry with
      | AddLetter { start; direction; deck; word } -> (
          let letter = Char.uppercase_ascii (UChar.char_of c) in
          let current_player = List.nth players current_player_index in
          let current_deck = current_player.letters in
          try
            let new_deck = consume_letter letter current_deck in
            LoopResultUpdateState
              (Gameplay
                 {
                   current_state with
                   players =
                     Util.set players current_player_index
                       { current_player with letters = new_deck };
                   entry =
                     AddLetter
                       {
                         start;
                         direction;
                         deck;
                         word = word @ [ letter ];
                       };
                 })
          with Not_found -> LoopResultContinue)
      | _ -> LoopResultContinue)
  | _ -> LoopResultContinue

let loop_victory (evt : LTerm_event.t) (victory_state : victory_state) :
    loop_result =
  match evt with
  | LTerm_event.Key _ ->
      let dictionary = Lazy.force global_dictionary in
      LoopResultUpdateState
        (Gameplay
           (blank_gameplay_state victory_state.players dictionary))
  | _ -> LoopResultContinue

let loop_title (evt : LTerm_event.t) (title_state : title_state) :
    loop_result =
  match evt with
  | LTerm_event.Key { code = LTerm_key.Char c; control = false; _ } ->
      let c = UChar.char_of c in
      let c = String.make 1 c in
      let rec update_last_player_name players =
        match players with
        | [] -> [ { name = c; points = 0; letters = new_deck () } ]
        | [ last ] -> [ { last with name = last.name ^ c } ]
        | current :: remainder ->
            current :: update_last_player_name remainder
      in
      LoopResultUpdateState
        (Title { players = update_last_player_name title_state.players })
  | LTerm_event.Key { code = LTerm_key.Char c; control = true; _ }
    when UChar.char_of c == 'c' ->
      LoopResultExit
  | LTerm_event.Key { code = LTerm_key.Enter; _ } ->
      if
        List.length title_state.players > 1
        && List.for_all
             (fun player -> player.name != "")
             title_state.players
      then
        LoopResultUpdateState
          (Gameplay
             (blank_gameplay_state title_state.players
                (Lazy.force global_dictionary)))
      else LoopResultContinue
  | LTerm_event.Key { code = LTerm_key.Tab; _ } ->
      LoopResultUpdateState
        (Title
           {
             players =
               title_state.players
               @ [ { name = ""; points = 0; letters = new_deck () } ];
           })
  | LTerm_event.Key { code = LTerm_key.Delete; _ } ->
      let rec remove_last_player players =
        match players with
        | [] -> []
        (* if only one player remains, clear their name instead of
           deleting them *)
        | [ player ] -> [ { player with name = "" } ]
        (* if two players are in the list, drop the last one*)
        | [ current; _ ] -> [ current ]
        | current :: remainder ->
            current :: remove_last_player remainder
      in
      LoopResultUpdateState
        (Title { players = remove_last_player title_state.players })
  | LTerm_event.Key { code = LTerm_key.Backspace; _ } ->
      let rec update_last_player_name players =
        match players with
        | [] -> []
        | [ last ] ->
            if String.length last.name > 0 then
              [
                {
                  last with
                  name =
                    String.sub last.name 0 (String.length last.name - 1);
                };
              ]
            else [ last ]
        | current :: remainder ->
            current :: update_last_player_name remainder
      in
      LoopResultUpdateState
        (Title { players = update_last_player_name title_state.players })
  | _ -> LoopResultContinue

(** The game loop. This loop runs for as long as the game is running,
    and changes the game's state in response to events. *)
let rec loop (ui : LTerm_ui.t) (game_state : game_state ref) :
    unit Lwt.t =
  let* evt = LTerm_ui.wait ui in
  let current_state = !game_state in
  let loop_result =
    match current_state with
    | Gameplay gameplay_state -> loop_gameplay evt gameplay_state
    | Victory victory_state -> loop_victory evt victory_state
    | Title title_state -> loop_title evt title_state
  in
  match loop_result with
  | LoopResultExit -> return_unit
  | LoopResultUpdateState new_state ->
      game_state := new_state;
      LTerm_ui.draw ui;
      loop ui game_state
  | LoopResultContinue -> loop ui game_state

let h_spacing = 3

let v_spacing = 2

(* [get_tile_screen_position ctx pan position] returns the position
   inside of [ctx] where a certain tile should be drawn given the
   panning of the board. *)
let get_tile_screen_position ctx (pan_row, pan_col) (row, col) =
  let ctx_size = LTerm_draw.size ctx in
  let center_row = ctx_size.rows / v_spacing / 2 in
  let center_col = ctx_size.cols / h_spacing / 2 in
  let row = row - pan_row + center_row in
  let col = col - pan_col + center_col in
  (row * v_spacing, col * h_spacing)

let draw_board_gridlines ctx pan =
  let size = LTerm_draw.size ctx in
  let width = size.cols in
  let height = size.rows in
  let style =
    { LTerm_style.none with foreground = Some LTerm_style.lblue }
  in

  (* draw hlines *)
  Util.range 0 height v_spacing (fun row ->
      LTerm_draw.draw_hline ctx row 0 width ~style LTerm_draw.Light);

  (* draw vlines *)
  Util.range 0 width h_spacing (fun col ->
      LTerm_draw.draw_vline ctx 0 col height ~style LTerm_draw.Light);

  (* draw center star *)
  let row, col = get_tile_screen_position ctx pan (0, 0) in
  LTerm_draw.draw_char ctx (row + 1) (col + 1) (Zed_char.of_utf8 "*")
    ~style:
      {
        LTerm_style.none with
        foreground = Some LTerm_style.blue;
        reverse = Some true;
      }

let draw_board_cursor ctx pan cursor =
  let row, col = get_tile_screen_position ctx pan cursor in
  let style = { LTerm_style.none with reverse = Some true } in
  try
    LTerm_draw.set_style
      (LTerm_draw.point ctx (row + 1) (col + 1))
      style;
    LTerm_draw.set_style
      (LTerm_draw.point ctx (row + 1) (col + 2))
      style
  with
  | LTerm_draw.Out_of_bounds -> ()
  | exn -> raise exn

let draw_board_tiles ctx pan tiles =
  Seq.iter
    (fun (position, letter) ->
      let row, col = get_tile_screen_position ctx pan position in
      LTerm_draw.draw_char ctx (row + 1) (col + 1)
        ~style:{ LTerm_style.none with bold = Some true }
        (Zed_char.of_utf8 (String.make 1 letter));
      LTerm_draw.draw_char ctx (row + 1) (col + 2)
        (Zed_char.of_utf8 " "))
    (Hashtbl.to_seq tiles)

let multiplier_at_position ((row, col) : position) : multiplier option =
  let row = wrap row 17 in
  let col = wrap col 17 in

  let row = abs row in
  let col = abs col in
  if row = col then Some DoubleWord
  else if row - col = 2 then Some TripleWord
  else if row - col = 6 then Some DoubleLet
  else if row - col = 7 then Some TripleLet
  else None

let print_multi (m_type : multiplier) =
  match m_type with
  | DoubleLet -> "DL"
  | DoubleWord -> "DW"
  | TripleLet -> "TL"
  | TripleWord -> "TW"

let get_multi_color (m_type : multiplier) =
  match m_type with
  | DoubleLet -> Some LTerm_style.cyan
  | DoubleWord -> Some LTerm_style.magenta
  | TripleLet -> Some LTerm_style.green
  | TripleWord -> Some LTerm_style.red

let draw_multipliers ctx pan =
  let size = LTerm_draw.size ctx in
  let screen_width = size.cols in
  let screen_height = size.rows in
  let board_width = screen_width / h_spacing in
  let board_height = screen_height / v_spacing in
  let pan_y, pan_x = pan in
  for
    board_col = (-board_width / 2) + pan_x to (board_width / 2) + pan_x
  do
    for
      board_row = (-board_height / 2) + pan_y
      to (board_height / 2) + pan_y
    do
      match multiplier_at_position (board_row, board_col) with
      | None -> ()
      | Some mult ->
          let row, col =
            get_tile_screen_position ctx pan (board_row, board_col)
          in
          LTerm_draw.draw_string ctx (row + 1) (col + 1)
            (Zed_string.of_utf8 (print_multi mult))
            ~style:
              {
                LTerm_style.none with
                background = get_multi_color mult;
              }
    done
  done

let draw_entry_highlight
    ctx
    pan
    (start : position)
    (direction : direction) =
  let row_increment, col_increment =
    match direction with
    | Up -> (-v_spacing, 0)
    | Down -> (v_spacing, 0)
    | Left -> (0, -h_spacing)
    | Right -> (0, h_spacing)
  in
  let row, col = get_tile_screen_position ctx pan start in
  let rec helper (row, col) =
    try
      LTerm_draw.set_style
        (LTerm_draw.point ctx (row + 1) (col + 1))
        { LTerm_style.none with reverse = Some true };
      LTerm_draw.set_style
        (LTerm_draw.point ctx (row + 1) (col + 2))
        { LTerm_style.none with reverse = Some true };
      helper (row + row_increment, col + col_increment)
    with LTerm_draw.Out_of_bounds -> ()
  in
  helper (row, col)

let draw_entry_tiles
    ctx
    pan
    tiles
    (start : position)
    (direction : direction)
    (word : char list) =
  let letter_positions =
    get_entry_letter_board_positions tiles start direction word
  in
  List.iter
    (fun (letter, position) ->
      let row, col = get_tile_screen_position ctx pan position in
      LTerm_draw.draw_char ctx (row + 1) (col + 1)
        (Zed_char.of_utf8 (String.make 1 letter))
        ~style:
          {
            LTerm_style.none with
            foreground = Some LTerm_style.magenta;
          };
      LTerm_draw.draw_char ctx (row + 1) (col + 2)
        (Zed_char.of_utf8 " ")
        ~style:
          {
            LTerm_style.none with
            foreground = Some LTerm_style.magenta;
          })
    letter_positions

let draw_instructions ctx instructions =
  let ctx_size = LTerm_draw.size ctx in
  let ctx_rect =
    { row1 = 0; row2 = ctx_size.rows; col1 = 0; col2 = ctx_size.cols }
  in
  let ctx = LTerm_draw.sub ctx (inset ctx_rect 1) in
  let ctx_size = LTerm_draw.size ctx in
  let lines =
    instructions |> Instructions.text
    |> Util.text_wrap ctx_size.cols
    |> List.map Zed_string.of_utf8
  in
  List.iteri
    (fun row line -> LTerm_draw.draw_string ctx row 0 line)
    lines

(* draw letters to letter box given a player's letter deck [deck] *)
let draw_letters ctx deck =
  let ctx_size = LTerm_draw.size ctx in
  let rec inner lst =
    match lst with
    | [] -> ()
    | h :: t ->
        if inner t = () then
          LTerm_draw.draw_string_aligned ctx
            ((7 - List.length t) * (ctx_size.rows / 8))
            H_align_center
            (Zed_string.of_utf8 (String.make 1 h))
        else ()
  in
  inner (deck_to_letters deck)

(** [player_display player curr] is the string assigned to that player
    to be displayed in the box*)
let player_display player =
  String.concat " : " [ player.name; string_of_int player.points ]

(* draw players to players box given a list of game_state.players *)
let draw_players ctx (gameplay_state : gameplay_state) =
  let num_players = List.length gameplay_state.players in
  let rec inner idx = function
    | [] -> ()
    | h :: t ->
        inner (idx + 1) t;
        let name_points = player_display h in
        let ctx_size = LTerm_draw.size ctx in
        LTerm_draw.draw_string_aligned ctx
          ((num_players - List.length t)
          * (ctx_size.rows / (num_players + 1)))
          H_align_center
          (Zed_string.of_utf8 name_points)
          ~style:
            {
              LTerm_style.none with
              reverse = Some (idx = gameplay_state.current_player_index);
            }
  in

  inner 0 gameplay_state.players

(* draw words and character highlight to be put in the selection box *)
let draw_sel_word ctx idx word ind axis =
  let draw_arrow = function
    | Horizontal ->
        LTerm_draw.draw_char ctx idx 0 (Zed_char.of_utf8 "???")
    | Vertical -> LTerm_draw.draw_char ctx idx 0 (Zed_char.of_utf8 "???")
  in
  draw_arrow axis;
  let str_lst = List.of_seq (String.to_seq word) in
  let draw_char ch col =
    LTerm_draw.draw_char ctx idx (col + 1) (Zed_char.of_utf8 ch)
  in
  let draw_highlighted_char ch col =
    LTerm_draw.draw_char ctx idx (col + 1) (Zed_char.of_utf8 ch)
      ~style:
        { LTerm_style.none with background = Some LTerm_style.cyan }
  in
  List.iteri
    (fun col ch ->
      if col = ind then draw_highlighted_char (String.make 1 ch) col
      else draw_char (String.make 1 ch) col)
    str_lst;
  let points = word_points word in
  LTerm_draw.draw_string ctx idx
    (List.length str_lst + 1)
    (Zed_string.of_utf8 (Printf.sprintf " (%d)" points))

(* draw selection box to give information about words and letter
   highlighted *)
let draw_selection ctx (gameplay_state : gameplay_state) =
  let { board; _ } = gameplay_state in
  let tile_to_str tile =
    match tile with None -> "" | Some c -> String.make 1 c
  in
  let current_words = Board.get_words_at board board.cursor in
  let find_ch_ind axis =
    let i = ref 0 in
    let str ind =
      match axis with
      | Vertical ->
          tile_to_str
            (get_tile board (fst board.cursor - ind, snd board.cursor))
      | Horizontal ->
          tile_to_str
            (get_tile board (fst board.cursor, snd board.cursor - ind))
    in
    while str (!i + 1) != "" do
      i := !i + 1
    done;
    !i
  in
  List.iteri
    (fun idx el ->
      let word = fst el in
      let axis = snd el in
      draw_sel_word ctx idx word (find_ch_ind axis) axis)
    current_words

let with_grid_cell ctx layout_spec row_start row_span col_start col_span
    =
  let ctx_size = LTerm_draw.size ctx in
  let ctx_rect =
    { row1 = 0; row2 = ctx_size.rows; col1 = 0; col2 = ctx_size.cols }
  in
  let abs_bounds =
    get_grid_rect ctx_rect layout_spec row_start row_span col_start
      col_span
  in
  let ctx = LTerm_draw.sub ctx abs_bounds in
  ctx

let with_frame ctx label connection =
  let ctx_size = LTerm_draw.size ctx in
  let ctx_rect =
    { row1 = 0; row2 = ctx_size.rows; col1 = 0; col2 = ctx_size.cols }
  in
  LTerm_draw.draw_frame_labelled ctx ctx_rect
    (Zed_string.of_utf8 label)
    connection;
  let ctx = LTerm_draw.sub ctx (inset ctx_rect 1) in
  ctx

let draw_gameplay ui_terminal matrix (gameplay_state : gameplay_state) =
  let size = LTerm_ui.size ui_terminal in
  let ctx = LTerm_draw.context matrix size in
  LTerm_draw.clear ctx;
  LTerm_draw.draw_hline ctx 0 0 size.cols LTerm_draw.Heavy;
  LTerm_draw.draw_string_aligned ctx 0 H_align_center
    ~style:{ LTerm_style.none with bold = Some true }
    (Zed_string.of_utf8 " koiiword ");
  if size.rows < 15 || size.cols < 30 then
    LTerm_draw.draw_string_aligned ctx (size.rows / 2) H_align_center
      (Zed_string.of_utf8 "please make \n your terminal bigger")
  else
    let rect =
      { row1 = 1; col1 = 0; row2 = size.rows - 1; col2 = size.cols }
    in
    let layout_spec = { cols = [ 0.3; 0.7 ]; rows = [ 0.4; 0.6 ] } in
    let ctx = LTerm_draw.sub ctx rect in
    (* draw board *)
    (let ctx = with_grid_cell ctx layout_spec 0 2 1 2 in
     let ctx = with_frame ctx " board " LTerm_draw.Heavy in
     let { board; _ } = gameplay_state in
     let { pan; cursor; tiles; _ } = board in
     draw_board_gridlines ctx pan;
     draw_multipliers ctx pan;
     draw_board_cursor ctx pan cursor;
     draw_board_tiles ctx pan tiles;
     match gameplay_state.entry with
     | AddLetter { start; direction; word; _ } ->
         draw_entry_highlight ctx pan start direction;
         draw_entry_tiles ctx pan tiles start direction word
     | _ -> ());
    (* draw players box *)
    (let ctx = with_grid_cell ctx layout_spec 0 1 0 1 in
     let _ = with_frame ctx " players " LTerm_draw.Heavy in
     draw_players ctx gameplay_state);
    (* draw letters box *)
    (let ctx = with_grid_cell ctx layout_spec 1 2 0 1 in
     let ctx = with_frame ctx " letters " LTerm_draw.Heavy in
     let { players; current_player_index; _ } = gameplay_state in
     let { letters; _ } = List.nth players current_player_index in
     draw_letters ctx letters);
    (* draw prompt box *)
    (let ctx = with_grid_cell ctx layout_spec 2 1 1 1 in
     let ctx = with_frame ctx "" LTerm_draw.Heavy in
     let { instructions; _ } = gameplay_state in
     draw_instructions ctx instructions);
    (* draw selection box *)
    let ctx = with_grid_cell ctx layout_spec 2 1 2 1 in
    let ctx = with_frame ctx " selection " LTerm_draw.Heavy in
    draw_selection ctx gameplay_state

let draw_victory ui_terminal matrix (victory_state : victory_state) =
  let size = LTerm_ui.size ui_terminal in
  let ctx = LTerm_draw.context matrix size in
  LTerm_draw.clear ctx;
  LTerm_draw.draw_hline ctx 0 0 size.cols LTerm_draw.Heavy;
  LTerm_draw.draw_string_aligned ctx 0 H_align_center
    ~style:{ LTerm_style.none with bold = Some true }
    (Zed_string.of_utf8 " koiiword ");
  if size.rows < 15 || size.cols < 30 then
    LTerm_draw.draw_string_aligned ctx (size.rows / 2) H_align_center
      (Zed_string.of_utf8 "please make \n your terminal bigger")
  else
    let rect =
      { row1 = 1; col1 = 0; row2 = size.rows - 1; col2 = size.cols }
    in
    let ctx = LTerm_draw.sub ctx rect in
    LTerm_draw.draw_string_aligned ctx (size.rows / 2) H_align_center
      (Zed_string.of_utf8 (victory_state.winner.name ^ " has won"));
    LTerm_draw.draw_string_aligned ctx
      ((size.rows / 2) + 1)
      H_align_center
      (Zed_string.of_utf8 "press any key to restart")

let draw_title ui_terminal matrix (title_state : title_state) =
  let size = LTerm_ui.size ui_terminal in
  let ctx = LTerm_draw.context matrix size in
  LTerm_draw.clear ctx;

  if size.rows < 15 || size.cols < 30 then
    LTerm_draw.draw_string_aligned ctx (size.rows / 2) H_align_center
      (Zed_string.of_utf8 "please make \n your terminal bigger")
  else
    let layout_spec = { cols = [ 1.0 ]; rows = [ 0.3; 0.6 ] } in
    let logo =
      {|
    / ___      ___     ( ) ( )               ___      __      ___   /  
   //\ \     //   ) ) / / / / //  / /  / / //   ) ) //  ) ) //   ) /   
  //  \ \   //   / / / / / / //  / /  / / //   / / //      //   / /    
 //    \ \ ((___/ / / / / / ((__( (__/ / ((___/ / //      ((___/ /     
 |}
    in
    (let ctx = with_grid_cell ctx layout_spec 0 1 0 1 in
     let size = LTerm_draw.size ctx in
     LTerm_draw.draw_string_aligned ctx
       ((size.rows / 2) - 2)
       H_align_center
       ~style:{ LTerm_style.none with bold = Some true }
       (Zed_string.of_utf8 logo));

    (let ctx = with_grid_cell ctx layout_spec 1 1 0 1 in
     LTerm_draw.draw_string_aligned ctx 0 H_align_center
       (Zed_string.of_utf8 "type to enter the name of a player");
     LTerm_draw.draw_string_aligned ctx 1 H_align_center
       (Zed_string.of_utf8 "press [tab] to add another player");
     LTerm_draw.draw_string_aligned ctx 2 H_align_center
       (Zed_string.of_utf8 "press [delete] to remove the current player");
     LTerm_draw.draw_string_aligned ctx 3 H_align_center
       (Zed_string.of_utf8 "press [ctrl-c] to quit");
     LTerm_draw.draw_string_aligned ctx 4 H_align_center
       (Zed_string.of_utf8 "press [enter] to start the game"));

    let ctx = with_grid_cell ctx layout_spec 2 1 0 1 in
    let size = LTerm_draw.size ctx in
    let start_col = (size.cols / 2) - 15 in
    List.iteri
      (fun idx player ->
        let text_player_num = "P" ^ string_of_int (idx + 1) ^ ": " in
        LTerm_draw.draw_string ctx idx start_col
          ~style:{ LTerm_style.none with bold = Some true }
          (Zed_string.of_utf8 text_player_num);
        if player.name = "" then
          LTerm_draw.draw_string ctx idx (start_col + 4)
            ~style:
              {
                LTerm_style.none with
                foreground = Some LTerm_style.white;
              }
            (Zed_string.of_utf8 "<no name>")
        else
          LTerm_draw.draw_string ctx idx (start_col + 4)
            (Zed_string.of_utf8 player.name))
      title_state.players

let draw ui_terminal matrix game_state =
  match game_state with
  | Gameplay gameplay_state ->
      draw_gameplay ui_terminal matrix gameplay_state
  | Victory victory_state ->
      draw_victory ui_terminal matrix victory_state
  | Title title_state -> draw_title ui_terminal matrix title_state

let main () =
  Random.self_init ();

  let* term = Lazy.force LTerm.stdout in

  let game_state : game_state ref =
    ref
      (Title
         {
           players =
             [ { name = ""; points = 0; letters = new_deck () } ];
         })
  in

  let* ui =
    LTerm_ui.create term (fun ui_terminal matrix ->
        draw ui_terminal matrix !game_state)
  in
  Lwt.finalize
    (fun () -> loop ui game_state)
    (fun () -> LTerm_ui.quit ui)

let () = Lwt_main.run (main ())
