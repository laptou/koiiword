open Board
open Player
(* open Tile *)

type game_state = {
  board : board;
  players : player list;
}

(*let valid (t : tile) (p : player) = if List.mem t p.letters then true
  else false*)

(* let place_tile (b : board) (t : tile) (pl : player) = if valid t pl
   then set_tile b t else set_tile b ' ' *)
