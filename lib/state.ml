open Board
open Player
open Entry
open Dictionary
open Instructions

type gameplay_state = {
  board : board;
  players : player list;
  entry : entry_state;
  instructions : instructions_state;
  current_player_index : int;
  dict : dictionary;
}

type game_state =
  | Title
  | Gameplay of gameplay_state
  | Victory of player
