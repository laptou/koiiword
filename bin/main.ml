(*
 * move.ml
 * -------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Lambda-Term.
 *)

open Lwt
open LTerm_geom
open LTerm_key
open Koiiword.Board
open Koiiword.State
open Koiiword.Layout
open Koiiword.Generate_letters
open Koiiword.Player
open CamomileLibrary

(** The [loop_result] type describes the response of the gameplay loop
    to an event. Currently, the game can respond by doing nothing
    ([LoopResultContinue]), updating the gameplay state and re-rendering
    ([LoopResultUpdateState]), or exiting ([LoopResultExit]) *)
type loop_result =
  | LoopResultContinue
  | LoopResultUpdateState of game_state
  | LoopResultExit

(** The game loop. This loop runs for as long as the game is running,
    and changes the game's state in response to events. *)
let rec loop (ui : LTerm_ui.t) (game_state : game_state ref) :
    unit Lwt.t =
  let%lwt evt = LTerm_ui.wait ui in
  let current_state = !game_state in
  let { board; players } = current_state in
  let { cursor } = board in
  let loop_result : loop_result =
    match evt with
    | LTerm_event.Key { code = Up; _ } ->
        LoopResultUpdateState
          {
            current_state with
            board = { cursor = (fst cursor - 1, snd cursor) };
          }
    | LTerm_event.Key { code = Down; _ } ->
        LoopResultUpdateState
          {
            current_state with
            board = { cursor = (fst cursor + 1, snd cursor) };
          }
    | LTerm_event.Key { code = Left; _ } ->
        LoopResultUpdateState
          {
            current_state with
            board = { cursor = (fst cursor, snd cursor - 1) };
          }
    | LTerm_event.Key { code = Right; _ } ->
        LoopResultUpdateState
          {
            current_state with
            board = { cursor = (fst cursor, snd cursor + 1) };
          }
    | LTerm_event.Key { code = Enter; _ } ->
        LoopResultUpdateState
          { current_state with players = advance_players players }
    | LTerm_event.Key { code = Escape; _ } -> LoopResultExit
    | LTerm_event.Key { code = LTerm_key.Char c; control = true; _ }
      -> (
        match UChar.char_of c with
        | 'c' -> LoopResultExit
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

let draw_board_gridlines ctx =
  let rec range start stop step fn =
    fn start;
    if start + step < stop then range (start + step) stop step fn
  in
  let size = LTerm_draw.size ctx in
  let width = size.cols in
  let height = size.rows in
  let h_spacing = 3 in
  let v_spacing = 2 in
  let style =
    { LTerm_style.none with foreground = Some LTerm_style.lblue }
  in

  (* draw hlines *)
  range 0 height v_spacing (fun row ->
      LTerm_draw.draw_hline ctx row 0 width ~style LTerm_draw.Light);

  (* draw vlines *)
  range 0 width h_spacing (fun col ->
      LTerm_draw.draw_vline ctx 0 col height ~style LTerm_draw.Light)

(* draw letters to letter box given a player's letter [lst] *)
let rec draw_letters ctx lst =
  let ctx_size = LTerm_draw.size ctx in
  match lst with
  | [] -> ()
  | h :: t ->
      if draw_letters ctx t = () then
        LTerm_draw.draw_string_aligned ctx
          ((7 - List.length t) * (ctx_size.rows / 8))
          H_align_center
          (Zed_string.of_utf8 (String.make 1 h))
      else ()

let draw_players ctx player =
  let name_points =
    String.concat " : " [ player.name; string_of_int player.points ]
  in
  let ctx_size = LTerm_draw.size ctx in
  LTerm_draw.draw_string_aligned ctx (ctx_size.rows / 8) H_align_center
    (Zed_string.of_utf8 name_points)

let draw_board_cursor ctx (row, col) =
  let row, col = ((row * 2) + 1, (col * 3) + 1) in
  let style = { LTerm_style.none with reverse = Some true } in
  try
    LTerm_draw.set_style (LTerm_draw.point ctx row col) style;
    LTerm_draw.set_style (LTerm_draw.point ctx row (col + 1)) style
  with
  | LTerm_draw.Out_of_bounds -> ()
  | exn -> raise exn

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
    (* draw board *)
    let current_player = List.hd game_state.players in
    (let ctx = with_grid_cell ctx layout_spec 0 2 1 2 in
     let ctx = with_frame ctx " board " LTerm_draw.Heavy in
     draw_board_gridlines ctx;
     draw_board_cursor ctx game_state.board.cursor);
    (* draw players box *)
    (let ctx = with_grid_cell ctx layout_spec 0 1 0 1 in
     let _ = with_frame ctx " players " LTerm_draw.Heavy in
     draw_players ctx current_player);
    (* draw letters box *)
    (let ctx = with_grid_cell ctx layout_spec 1 2 0 1 in
     let _ = with_frame ctx " letters " LTerm_draw.Heavy in
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
  (* Define players *)
  let player1 = { name = "P1"; points = 50; letters = start_game [] } in
  let player2 = { name = "P2"; points = 30; letters = start_game [] } in
  let player_lst = [ player2; player1 ] in

  let%lwt term = Lazy.force LTerm.stdout in

  let game_state : game_state ref =
    ref { board = { cursor = (0, 0) }; players = player_lst }
  in

  let%lwt ui =
    LTerm_ui.create term (fun ui_terminal matrix ->
        draw ui_terminal matrix !game_state)
  in
  Lwt.finalize
    (fun () -> loop ui game_state)
    (fun () -> LTerm_ui.quit ui)

let () = Lwt_main.run (main ())
