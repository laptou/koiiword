open Board
open Player
open Entry
open Dictionary

type game_state = {
  board : board;
  players : player list;
  entry : entry_state;
  current_player_index : int;
  dict : dictionary;
  mutable in_play : bool;
}
