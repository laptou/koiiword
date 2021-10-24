open Tile
open Layout

type board = {
  cursor : position;
  tiles : (position, char) Hashtbl.t;
}

let set_tile board (tile : tile) =
  let ch, position = tile in
  if Hashtbl.mem board.tiles position then
    raise (Failure "there is a tile at this position already")
  else Hashtbl.add board.tiles position ch

let get_tile board position =
  try Some (Hashtbl.find board.tiles position) with Not_found -> None
