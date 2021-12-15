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

type victory_state = {
  winner : player;
  players : player list;
}

type title_state = {
  players : player list;
}

type game_state =
  | Title of title_state
  | Gameplay of gameplay_state
  | Victory of victory_state

let blank_gameplay_state player_lst dictionary =
  {
    board = new_board ();
    players = player_lst;
    entry = SelectStart;
    instructions = Instructions.StartGame;
    current_player_index = 0;
    dict = dictionary;
  }
