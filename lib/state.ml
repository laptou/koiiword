open Board
open Player
open Layout

(* entry state describes the stage of entering a word that the current
   player is at *)
type entry_state =
  (* they are selecting where the word they will enter should begin *)
  | SelectStart
  (* they are selecting which direction the word they will enter should
     travel *)
  | SelectDirection of position
  (* they are adding letters to this word *)
  | AddLetter of position * direction * char list

type game_state = {
  board : board;
  players : player list;
  entry : entry_state;
  current_player_index : int;
}
