open Board
open Player
open Entry

type game_state = {
  board : board;
  players : player list;
  entry : entry_state;
  current_player_index : int;
}
