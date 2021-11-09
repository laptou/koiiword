open Tile
open Layout
open Entry

type multiplier =
  | DoubleLet
  | DoubleWord
  | TripleLet
  | TripleWord

type board = {
  (* the location of the user's cursor *)
  cursor : position;
  (* the location that is shown at the center of the screen *)
  pan : position;
  (* the tiles on the board *)
  tiles : (position, char) Hashtbl.t;
  multipliers : (position, multiplier) Hashtbl.t;
}

type axis =
  | Horizontal
  | Vertical

let new_board () =
  {
    cursor = (0, 0);
    tiles = Hashtbl.create 0;
    pan = (0, 0);
    multipliers = Hashtbl.create 0;
  }

let set_tile board (tile : tile) =
  let ch, position = tile in
  if Hashtbl.mem board.tiles position then
    failwith "there is a tile at this position already"
  else Hashtbl.add board.tiles position ch

let get_tile board position =
  try Some (Hashtbl.find board.tiles position) with Not_found -> None

(* This exception is thrown when there are tiles on the board that are
   not connected to the tile at (0, 0) with other tiles. *)
exception Disconnected

let get_words_impl
    (board : board)
    (position : position)
    (max_depth : int)
    (check_connected : bool) : string list =
  let { tiles; _ } = board in
  if Hashtbl.length tiles = 0 then []
  else
    let seen = Hashtbl.create (Hashtbl.length tiles) in
    let rec partial_word_at
        (position : position)
        (axis : axis)
        (h_depth : int)
        (v_depth : int) : string * string list =
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
                partial_word_at below Vertical h_depth v_depth
              in
              let current_word =
                String.make 1 current_letter ^ partial_word
              in

              let branch_words =
                if h_depth < max_depth then
                  if
                    (not (Hashtbl.mem seen right))
                    && Hashtbl.mem tiles right
                  then
                    branch_words
                    @ search_words_at right Horizontal (h_depth + 1)
                        v_depth
                  else if
                    (not (Hashtbl.mem seen left))
                    && Hashtbl.mem tiles left
                  then
                    branch_words
                    @ search_words_at left Horizontal (h_depth + 1)
                        v_depth
                  else branch_words
                else branch_words
              in
              (current_word, branch_words)
          | Horizontal ->
              let partial_word, branch_words =
                partial_word_at right Horizontal h_depth v_depth
              in
              let current_word =
                String.make 1 current_letter ^ partial_word
              in

              let branch_words =
                if v_depth < max_depth then
                  if
                    (not (Hashtbl.mem seen below))
                    && Hashtbl.mem tiles below
                  then
                    branch_words
                    @ search_words_at below Vertical h_depth
                        (v_depth + 1)
                  else if
                    (not (Hashtbl.mem seen above))
                    && Hashtbl.mem tiles above
                  then
                    branch_words
                    @ search_words_at above Vertical h_depth
                        (v_depth + 1)
                  else branch_words
                else branch_words
              in
              (current_word, branch_words))
    and search_words_at
        (position : position)
        (axis : axis)
        (h_depth : int)
        (v_depth : int) : string list =
      let row, col = position in
      let above = (row - 1, col) in
      let left = (row, col - 1) in
      match axis with
      | Vertical ->
          if Hashtbl.mem tiles above then
            search_words_at above Vertical h_depth v_depth
          else
            let word, branch_words =
              partial_word_at position Vertical h_depth v_depth
            in
            word :: branch_words
      | Horizontal ->
          if Hashtbl.mem tiles left then
            search_words_at left Horizontal h_depth v_depth
          else
            let word, branch_words =
              partial_word_at position Horizontal h_depth v_depth
            in
            word :: branch_words
    in
    let row, col = position in
    let above = (row - 1, col) in
    let below = (row + 1, col) in
    let left = (row, col - 1) in
    let right = (row, col + 1) in
    let found_words =
      if Hashtbl.mem tiles above || Hashtbl.mem tiles below then
        search_words_at position Vertical 1 0
      else if Hashtbl.mem tiles left || Hashtbl.mem tiles right then
        search_words_at position Horizontal 0 1
      else []
    in
    if check_connected && Hashtbl.length seen < Hashtbl.length tiles
    then raise Disconnected
    else found_words

(** [get_words_deep board] returns all of the words that are detected on
    [board]. Raises [Disconnected] if it detects that any tiles are not
    connected to the tile at (0, 0). *)
let get_words_deep (board : board) : string list =
  get_words_impl board (0, 0) 10000000 true

(** [get_words_at board position max_depth] returns words that are found
    by starting at [position] and searching outwards. Each time the
    search branches, the depth increases, and it is capped at
    [max_depth]. *)
(* let get_words_at (board : board) (position : position) (max_depth :
   int) : string list = get_words_impl board position max_depth false *)

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
