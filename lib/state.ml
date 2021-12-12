open Board
open Player
open Entry
open Dictionary
open Instructions

type game_state = {
  board : board;
  players : player list;
  entry : entry_state;
  instructions : instructions_state;
  current_player_index : int;
  dict : dictionary;
}
