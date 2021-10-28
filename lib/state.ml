open Board
open Player
open Layout
open Generate_letters

(* entry state describes the stage of entering a word that the current
   player is at *)
type entry_state =
  (* they are selecting where the word they will enter should begin *)
  | SelectStart
  (* they are selecting which direction the word they will enter should
     travel *)
  | SelectDirection of { start : position }
  (* they are adding letters to this word *)
  | AddLetter of {
      (* the position where this word starts *)
      start : position;
      (* the direction that this word travels from the start *)
      direction : direction;
      (* the letter deck the current player had before adding any
         letters *)
      deck : letter_deck;
      (* the letters that have been added to this word so far *)
      word : char list;
    }

type game_state = {
  board : board;
  players : player list;
  entry : entry_state;
  current_player_index : int;
}
