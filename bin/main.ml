open Lwt
open LTerm_geom
open LTerm_key
open Koiiword
open Koiiword.Board
open Koiiword.State
open Koiiword.Layout
open Koiiword.Generate_letters
open Koiiword.Player
open Koiiword.Entry
open Koiiword.Dictionary
open CamomileLibrary

(** The [loop_result] type describes the response of the gameplay loop
    to an event. Currently, the game can respond by doing nothing
    ([LoopResultContinue]), updating the gameplay state and re-rendering
    ([LoopResultUpdateState]), or exiting ([LoopResultExit]) *)
type loop_result =
  | LoopResultContinue
  | LoopResultUpdateState of game_state
  | LoopResultExit

let with_input_direction
    (current_state : game_state)
    (start : position)
    (direction : direction) =
  let { players; current_player_index; _ } = current_state in
  let current_player = List.nth players current_player_index in
  let current_deck = current_player.letters in
  {
    current_state with
    entry =
      AddLetter { start; direction; deck = current_deck; word = [] };
  }

let with_deck (current_state : game_state) (new_deck : letter_deck) =
  let { players; current_player_index; _ } = current_state in
  let current_player = List.nth players current_player_index in
  {
    current_state with
    players =
      Util.set players current_player_index
        { current_player with letters = new_deck };
  }

(** The game loop. This loop runs for as long as the game is running,
    and changes the game's state in response to events. *)
let rec loop (ui : LTerm_ui.t) (game_state : game_state ref) :
    unit Lwt.t =
  let%lwt evt = LTerm_ui.wait ui in
  let current_state = !game_state in
  let { board; players; entry; current_player_index; dict } =
    current_state
  in
  let { cursor; _ } = board in
  let loop_result : loop_result =
    match evt with
    | LTerm_event.Key { code = Up; _ } -> (
        match entry with
        | SelectStart ->
            LoopResultUpdateState
              {
                current_state with
                board =
                  { board with cursor = (fst cursor - 1, snd cursor) };
              }
        | SelectDirection { start } ->
            LoopResultUpdateState
              (with_input_direction current_state start Up)
        | _ -> LoopResultContinue)
    | LTerm_event.Key { code = Down; _ } -> (
        match entry with
        | SelectStart ->
            LoopResultUpdateState
              {
                current_state with
                board =
                  { board with cursor = (fst cursor + 1, snd cursor) };
              }
        | SelectDirection { start } ->
            LoopResultUpdateState
              (with_input_direction current_state start Down)
        | _ -> LoopResultContinue)
    | LTerm_event.Key { code = Left; _ } -> (
        match entry with
        | SelectStart ->
            LoopResultUpdateState
              {
                current_state with
                board =
                  { board with cursor = (fst cursor, snd cursor - 1) };
              }
        | SelectDirection { start } ->
            LoopResultUpdateState
              (with_input_direction current_state start Left)
        | _ -> LoopResultContinue)
    | LTerm_event.Key { code = Right; _ } -> (
        match entry with
        | SelectStart ->
            LoopResultUpdateState
              {
                current_state with
                board =
                  { board with cursor = (fst cursor, snd cursor + 1) };
              }
        | SelectDirection { start } ->
            LoopResultUpdateState
              (with_input_direction current_state start Right)
        | _ -> LoopResultContinue)
    | LTerm_event.Key { code = Enter; _ } -> (
        match entry with
        | SelectStart -> (
            (* only allow the player to start a word here if there is no
               tile at this location*)
            match get_tile board cursor with
            | None ->
                LoopResultUpdateState
                  {
                    current_state with
                    entry = SelectDirection { start = cursor };
                  }
            | _ -> LoopResultContinue)
        | AddLetter { start; direction; word; deck; _ } ->
            let current_player =
              List.nth players current_player_index
            in
            let current_deck = current_player.letters in
            let new_deck = refill_deck current_deck in
            (* Create string version of word char list *)
            let str_word =
              List.fold_right
                (fun x acc -> String.make 1 x ^ acc)
                word ""
            in

            if is_word_valid dict str_word then
              (* If word is valid than accept it*)
              LoopResultUpdateState
                {
                  current_state with
                  players =
                    Util.set players current_player_index
                      { current_player with letters = new_deck };
                  current_player_index =
                    (current_player_index + 1) mod List.length players;
                  entry = SelectStart;
                  board =
                    {
                      board with
                      tiles =
                        apply_entry_tiles board.tiles start direction
                          word;
                    };
                }
            else
              (* If word is invalid, return their original deck and have
                 them back in reselect state*)
              LoopResultUpdateState
                {
                  (with_deck current_state deck) with
                  entry = SelectStart;
                }
        | _ -> LoopResultContinue)
    | LTerm_event.Key { code = Escape; _ } -> (
        match entry with
        (* if they were in the middle of spelling a word, restore the
           original deck *)
        | AddLetter { deck; _ } ->
            LoopResultUpdateState
              {
                (with_deck current_state deck) with
                entry = SelectStart;
              }
        (* otherwise, just switch back to select start mode *)
        | _ ->
            LoopResultUpdateState
              { current_state with entry = SelectStart })
    | LTerm_event.Key { code = LTerm_key.Char c; control = true; _ }
      -> (
        match UChar.char_of c with
        | 'c' -> LoopResultExit
        | _ -> LoopResultContinue)
    | LTerm_event.Key { code = LTerm_key.Char c; control = false; _ }
      -> (
        match entry with
        | AddLetter { start; direction; deck; word } -> (
            let letter = Char.uppercase_ascii (UChar.char_of c) in
            let current_player =
              List.nth players current_player_index
            in
            let current_deck = current_player.letters in
            try
              let new_deck = consume_letter letter current_deck in
              LoopResultUpdateState
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
                }
            with Not_found -> LoopResultContinue)
        | _ -> LoopResultContinue)
    | _ -> LoopResultContinue
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

let draw_board_gridlines ctx =
  let rec range start stop step fn =
    fn start;
    if start + step < stop then range (start + step) stop step fn
  in
  let size = LTerm_draw.size ctx in
  let width = size.cols in
  let height = size.rows in
  let style =
    { LTerm_style.none with foreground = Some LTerm_style.lblue }
  in

  (* draw hlines *)
  range 0 height v_spacing (fun row ->
      LTerm_draw.draw_hline ctx row 0 width ~style LTerm_draw.Light);

  (* draw vlines *)
  range 0 width h_spacing (fun col ->
      LTerm_draw.draw_vline ctx 0 col height ~style LTerm_draw.Light)

let draw_board_cursor ctx (row, col) =
  let row, col = ((row * v_spacing) + 1, (col * h_spacing) + 1) in
  let style = { LTerm_style.none with reverse = Some true } in
  try
    LTerm_draw.set_style (LTerm_draw.point ctx row col) style;
    LTerm_draw.set_style (LTerm_draw.point ctx row (col + 1)) style
  with
  | LTerm_draw.Out_of_bounds -> ()
  | exn -> raise exn

let draw_board_tiles ctx tiles =
  Seq.iter
    (fun (position, letter) ->
      let row, col = position in
      LTerm_draw.draw_char ctx
        ((row * v_spacing) + 1)
        ((col * h_spacing) + 1)
        (Zed_char.of_utf8 (String.make 1 letter)))
    (Hashtbl.to_seq tiles)

let draw_entry_highlight ctx (start : position) (direction : direction)
    =
  let rec helper (row, col) (row_increment, col_increment) =
    try
      LTerm_draw.set_style
        (LTerm_draw.point ctx
           ((row * v_spacing) + 1)
           ((col * h_spacing) + 1))
        { LTerm_style.none with reverse = Some true };
      LTerm_draw.set_style
        (LTerm_draw.point ctx
           ((row * v_spacing) + 1)
           ((col * h_spacing) + 2))
        { LTerm_style.none with reverse = Some true };
      helper
        (row + row_increment, col + col_increment)
        (row_increment, col_increment)
    with LTerm_draw.Out_of_bounds -> ()
  in
  let increment =
    match direction with
    | Up -> (-1, 0)
    | Down -> (1, 0)
    | Left -> (0, -1)
    | Right -> (0, 1)
  in
  helper start increment

let draw_entry_tiles
    ctx
    tiles
    (start : position)
    (direction : direction)
    (word : char list) =
  let letter_positions =
    get_entry_letter_positions tiles start direction word
  in
  List.iter
    (fun (letter, (row, col)) ->
      LTerm_draw.draw_char ctx
        ((row * v_spacing) + 1)
        ((col * h_spacing) + 1)
        (Zed_char.of_utf8 (String.make 1 letter))
        ~style:
          {
            LTerm_style.none with
            foreground = Some LTerm_style.magenta;
          })
    letter_positions

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
let draw_players ctx players current_player_index =
  let num_players = List.length players in
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
              reverse = Some (idx = current_player_index);
            }
  in

  inner 0 players

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

let layout_spec = { cols = [ 0.3; 0.7 ]; rows = [ 0.4; 0.6 ] }

(** [sort_players players] is a list of players types sorted
    lexicographically by name. *)
let sort_players players =
  let comp p1 p2 = Stdlib.compare p1.name p2.name in
  List.sort comp players

(** The renderer. This takes game state and a terminal UI object and
    renders the game according to its current state. *)
let draw ui_terminal matrix (game_state : game_state) =
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
    let players = game_state.players in
    (* draw board *)
    let current_player_index = game_state.current_player_index in
    (let ctx = with_grid_cell ctx layout_spec 0 2 1 2 in
     let ctx = with_frame ctx " board " LTerm_draw.Heavy in
     draw_board_gridlines ctx;
     draw_board_cursor ctx game_state.board.cursor;
     draw_board_tiles ctx game_state.board.tiles;
     match game_state.entry with
     | AddLetter { start; direction; word; _ } ->
         draw_entry_highlight ctx start direction;
         draw_entry_tiles ctx game_state.board.tiles start direction
           word
     | _ -> ());
    (* draw players box *)
    (let ctx = with_grid_cell ctx layout_spec 0 1 0 1 in
     let _ = with_frame ctx " players " LTerm_draw.Heavy in
     draw_players ctx players current_player_index);
    (* draw letters box *)
    (let ctx = with_grid_cell ctx layout_spec 1 2 0 1 in
     let _ = with_frame ctx " letters " LTerm_draw.Heavy in
     let current_player =
       List.nth game_state.players current_player_index
     in
     draw_letters ctx current_player.letters);
    (* draw prompt box *)
    (let ctx = with_grid_cell ctx layout_spec 2 1 1 1 in
     let _ = with_frame ctx "" LTerm_draw.Heavy in
     ());
    (* draw selection box *)
    let ctx = with_grid_cell ctx layout_spec 2 1 2 1 in
    let _ = with_frame ctx " selection " LTerm_draw.Heavy in
    ()

let main () =
  Random.self_init ();

  let dictionary = dict_from_file "dictionary/english_dict.txt" in
  (* Define players *)
  let player1 = { name = "P1"; points = 50; letters = new_deck () } in
  let player2 = { name = "P2"; points = 30; letters = new_deck () } in
  let player3 = { name = "P3"; points = 30; letters = new_deck () } in
  let player4 = { name = "P4"; points = 30; letters = new_deck () } in
  let player_lst = [ player1; player2; player3; player4 ] in

  let%lwt term = Lazy.force LTerm.stdout in

  let game_state : game_state ref =
    ref
      {
        board = { cursor = (0, 0); tiles = Hashtbl.create 100 };
        players = sort_players player_lst;
        entry = SelectStart;
        current_player_index = 0;
        dict = dictionary;
      }
  in

  let%lwt ui =
    LTerm_ui.create term (fun ui_terminal matrix ->
        draw ui_terminal matrix !game_state)
  in
  Lwt.finalize
    (fun () -> loop ui game_state)
    (fun () -> LTerm_ui.quit ui)

let () = Lwt_main.run (main ())
