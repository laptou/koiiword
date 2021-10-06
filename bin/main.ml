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

(* open LTerm_text *)
open LTerm_key
open Koiiword.Board
open Koiiword.State

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
  let { board } = current_state in
  let { cursor } = board in
  let loop_result : loop_result =
    match evt with
    | LTerm_event.Key { code = Up; _ } ->
        LoopResultUpdateState
          { board = { cursor = (fst cursor - 1, snd cursor) } }
    | LTerm_event.Key { code = Down; _ } ->
        LoopResultUpdateState
          { board = { cursor = (fst cursor + 1, snd cursor) } }
    | LTerm_event.Key { code = Left; _ } ->
        LoopResultUpdateState
          { board = { cursor = (fst cursor, snd cursor - 1) } }
    | LTerm_event.Key { code = Right; _ } ->
        LoopResultUpdateState
          { board = { cursor = (fst cursor, snd cursor + 1) } }
    | LTerm_event.Key { code = Escape; _ } -> LoopResultExit
    | _ -> LoopResultContinue
  in
  match loop_result with
  | LoopResultExit -> return_unit
  | LoopResultUpdateState new_state ->
      game_state := new_state;
      LTerm_ui.draw ui;
      loop ui game_state
  | LoopResultContinue -> loop ui game_state

let draw_main_boxes ctx dim title =
  LTerm_draw.draw_frame_labelled ctx dim ~alignment:H_align_center
    (Zed_string.of_utf8 title)
    LTerm_draw.Light

(** The renderer. This takes game state and a terminal UI object and
    renders the game according to its current state. *)
let draw ui_terminal matrix (game_state : game_state) =
  let size = LTerm_ui.size ui_terminal in
  let ctx = LTerm_draw.context matrix size in
  LTerm_draw.clear ctx;
  draw_main_boxes ctx
    {
      row1 = 0;
      col1 = size.cols * 4 / 20;
      row2 = size.rows * 17 / 20;
      col2 = size.cols;
    }
    "Koiiword";
  draw_main_boxes ctx
    {
      row1 = 0;
      col1 = 0;
      row2 = size.rows * 8 / 20;
      col2 = size.cols * 4 / 20;
    }
    "Players";
  draw_main_boxes ctx
    {
      row1 = size.rows * 8 / 20;
      col1 = 0;
      row2 = size.rows;
      col2 = size.cols * 4 / 20;
    }
    "Letters";
  draw_main_boxes ctx
    {
      row1 = size.rows * 17 / 20;
      col1 = size.cols * 13 / 20;
      row2 = size.rows;
      col2 = size.cols;
    }
    "Selection";
  draw_main_boxes ctx
    {
      row1 = size.rows * 17 / 20;
      col1 = size.cols * 4 / 20;
      row2 = size.rows;
      col2 = size.cols * 13 / 20;
    }
    "Instructions";
  if size.rows > 2 && size.cols > 2 then
    let ctx =
      LTerm_draw.sub ctx
        {
          row1 = 1;
          col1 = 1;
          row2 = size.rows - 1;
          col2 = size.cols - 1;
        }
    in
    let row, col = game_state.board.cursor in
    let style = { LTerm_style.none with reverse = Some true } in
    LTerm_draw.draw_char ctx row col ~style (Zed_char.of_utf8 "*")

let main () =
  let%lwt term = Lazy.force LTerm.stdout in

  let game_state : game_state ref =
    ref { board = { cursor = (0, 0) } }
  in

  let%lwt ui =
    LTerm_ui.create term (fun ui_terminal matrix ->
        draw ui_terminal matrix !game_state)
  in
  Lwt.finalize
    (fun () -> loop ui game_state)
    (fun () -> LTerm_ui.quit ui)

let () = Lwt_main.run (main ())
