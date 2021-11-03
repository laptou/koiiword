open Layout
type board = {
  (* the location of the user's cursor *)
  cursor : position;
  (* the location that is shown at the center of the screen *)
  pan : position;
  (* the tiles on the board *)
  tiles : (position, char) Hashtbl.t;
}
type axis = Horizontal | Vertical
val new_board : unit -> board
val set_tile : board -> Tile.tile -> unit
val get_tile : board -> position -> char option
exception Disconnected
val get_words_deep : board -> string list
(* val get_words_at : board -> Layout.position -> int -> string list *)
val apply_entry_tiles :
  (position, char) Hashtbl.t ->
  position ->
  Entry.direction -> char list -> (position, char) Hashtbl.t
