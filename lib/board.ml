open Tile
open Layout
open Entry

type board = {
  (* the location of the user's cursor *)
  cursor : position;
  (* the location that is shown at the center of the screen *)
  pan : position;
  (* the tiles on the board *)
  tiles : (position, char) Hashtbl.t;
}

type axis =
  | Horizontal
  | Vertical

let new_board () =
  { cursor = (0, 0); tiles = Hashtbl.create 0; pan = (0, 0) }

let set_tile board (tile : tile) =
  let ch, position = tile in
  if Hashtbl.mem board.tiles position then
    failwith "there is a tile at this position already"
  else Hashtbl.add board.tiles position ch

let get_tile board position =
  try Some (Hashtbl.find board.tiles position) with Not_found -> None

let get_words (board : board) : string list =
  let { tiles; _ } = board in
  if Hashtbl.length tiles = 0 then []
  else
    let seen = Hashtbl.create (Hashtbl.length tiles) in
    let rec partial_word_at (position : position) (axis : axis) :
        string * string list =
      let row, col = position in
      match Hashtbl.find_opt tiles position with
      | None -> ("", [])
      | Some current_letter -> (
          Hashtbl.add seen position ();
          let above = (row - 1, col) in
          let below = (row + 1, col) in
          let left = (row, col - 1) in
          let right = (row, col + 1) in
          match axis with
          | Vertical ->
              let partial_word, branch_words =
                partial_word_at below Vertical
              in
              let current_word =
                String.make 1 current_letter ^ partial_word
              in

              let branch_words =
                if
                  (not (Hashtbl.mem seen right))
                  && Hashtbl.mem tiles right
                then branch_words @ search_words_at right Horizontal
                else if
                  (not (Hashtbl.mem seen left))
                  && Hashtbl.mem tiles left
                then branch_words @ search_words_at left Horizontal
                else branch_words
              in
              (current_word, branch_words)
          | Horizontal ->
              let partial_word, branch_words =
                partial_word_at right Horizontal
              in
              let current_word =
                String.make 1 current_letter ^ partial_word
              in

              let branch_words =
                if
                  (not (Hashtbl.mem seen below))
                  && Hashtbl.mem tiles below
                then branch_words @ search_words_at below Vertical
                else if
                  (not (Hashtbl.mem seen above))
                  && Hashtbl.mem tiles above
                then branch_words @ search_words_at above Vertical
                else branch_words
              in

              (current_word, branch_words))
    and search_words_at (position : position) (axis : axis) :
        string list =
      let row, col = position in
      let above = (row - 1, col) in
      let left = (row, col - 1) in
      match axis with
      | Vertical ->
          if Hashtbl.mem tiles above then search_words_at above Vertical
          else
            let word, branch_words =
              partial_word_at position Vertical
            in
            word :: branch_words
      | Horizontal ->
          if Hashtbl.mem tiles left then search_words_at left Horizontal
          else
            let word, branch_words =
              partial_word_at position Horizontal
            in
            word :: branch_words
    in
    search_words_at (0, 0) Vertical

let apply_entry_tiles
    tiles
    (start : position)
    (direction : direction)
    (word : char list) =
  let letter_positions =
    get_entry_letter_board_positions tiles start direction word
  in
  List.iter
    (fun (letter, position) -> Hashtbl.add tiles position letter)
    letter_positions;
  tiles
