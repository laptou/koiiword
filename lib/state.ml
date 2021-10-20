open Board
open Player
(* open Tile *)

type game_state = {
  board : board;
  players : player list;
  current_player : player;
}

(** [next_player curr_player players] returns the next player given the
    current player *)
let next_player curr_player players =
  let rec next_player_inner curr_play player_lst = function
    | [] -> failwith "no players"
    | h :: t ->
        if t = [] then List.nth players 0
        else if h.name = curr_player.name then List.nth t 0
        else next_player_inner curr_play player_lst t
  in
  next_player_inner curr_player players players
