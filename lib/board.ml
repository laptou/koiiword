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
        search_words_at position Vertical 0 1
      else if Hashtbl.mem tiles left || Hashtbl.mem tiles right then
        search_words_at position Horizontal 1 0
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
let get_words_at (board : board) (position : position) : string list =
  let { tiles; _ } = board in
  if Hashtbl.length tiles = 0 then []
  else
    let rec read_word (position : position) (axis : axis) : string =
      let row, col = position in
      match Hashtbl.find_opt tiles position with
      | Some l -> (
          let l = String.make 1 l in
          match axis with
          | Vertical ->
              let below = (row + 1, col) in
              l ^ read_word below axis
          | Horizontal ->
              let right = (row, col + 1) in
              l ^ read_word right axis)
      | None -> ""
    in

    let rec find_word (position : position) (axis : axis) :
        string option =
      let row, col = position in
      if Hashtbl.mem tiles position then
        match axis with
        | Vertical ->
            let above = (row - 1, col) in
            if Hashtbl.mem tiles above then find_word above axis
            else Some (read_word position axis)
        | Horizontal ->
            let left = (row, col - 1) in
            if Hashtbl.mem tiles left then find_word left axis
            else Some (read_word position axis)
      else None
    in

    List.filter_map
      (fun word ->
        match word with
        | Some word ->
            if String.length word > 1 then Some word else None
        | _ -> None)
      [ find_word position Vertical; find_word position Horizontal ]

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
