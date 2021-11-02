open Board
open Player

type game_state = {
  board : board;
  players : player list;
  current_player_index : int;
  words : string list;
}
