open Layout
open Generate_letters

type direction =
  | Up
  | Down
  | Left
  | Right

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

(** gets the position of the letters in the draft entry on the board *)
let get_entry_letter_board_positions
    tiles
    (start : position)
    (direction : direction)
    (word : char list) : (char * position) list =
  let rec helper (row, col) (row_increment, col_increment) tiles letters
      =
    if Hashtbl.mem tiles (row, col) then
      helper
        (row + row_increment, col + col_increment)
        (row_increment, col_increment)
        tiles letters
    else
      match letters with
      | letter :: letters ->
          (letter, (row, col))
          :: helper
               (row + row_increment, col + col_increment)
               (row_increment, col_increment)
               tiles letters
      | [] -> []
  in
  let increment =
    match direction with
    | Up -> (-1, 0)
    | Down -> (1, 0)
    | Left -> (0, -1)
    | Right -> (0, 1)
  in
  helper start increment tiles word
