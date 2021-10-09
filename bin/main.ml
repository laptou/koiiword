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

let draw_grid ctx rect =
  let rec range start stop step fn =
    fn start;
    if start + step < stop then range (start + step) stop step fn
  in
  let width = rect.col2 - rect.col1 in
  let height = rect.row2 - rect.row1 in
  let spacing = 2 in
  let style =
    { LTerm_style.none with foreground = Some LTerm_style.green }
  in

  (* draw hlines *)
  range rect.row1 rect.row2 spacing (fun row ->
      LTerm_draw.draw_hline ctx (rect.row1 + row) rect.col1 width ~style
        LTerm_draw.Light);

  (* draw vlines *)
  range rect.col1 rect.col2 spacing (fun col ->
      LTerm_draw.draw_vline ctx rect.row1 (rect.col1 + col) height
        ~style LTerm_draw.Light)

(** The renderer. This takes game state and a terminal UI object and
    renders the game according to its current state. *)
let draw ui_terminal matrix (game_state : game_state) =
  let size = LTerm_ui.size ui_terminal in
  let ctx = LTerm_draw.context matrix size in
  LTerm_draw.clear ctx;
  LTerm_draw.draw_hline ctx 0 0 size.cols LTerm_draw.Heavy;
  LTerm_draw.draw_string_aligned ctx 0 H_align_center
    (Zed_string.of_utf8 " koiiword ");
  LTerm_draw.draw_frame ctx
    { row1 = 1; col1 = 1; row2 = size.rows - 1; col2 = size.cols - 1 }
    LTerm_draw.Heavy;
  if size.rows > 2 && size.cols > 2 then (
    let ctx =
      LTerm_draw.sub ctx
        {
          row1 = 1;
          col1 = 1;
          row2 = size.rows - 1;
          col2 = size.cols - 1;
        }
    in
    let size = LTerm_draw.size ctx in
    draw_grid ctx
      { row1 = 1; col1 = 1; row2 = size.rows - 1; col2 = size.cols - 1 };
    let row, col = game_state.board.cursor in
    let style = { LTerm_style.none with reverse = Some true } in
    LTerm_draw.draw_char ctx
      ((row * 2) + 1)
      ((col * 2) + 1)
      ~style (Zed_char.of_utf8 "*"))

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
