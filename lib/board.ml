open Tile
open Layout
(* open LTerm_geom *)

type board = {
  cursor : position;
  tiles : tile list;
}

(* let set_tile (b : board) (t : tile) = { cursor = b.cursor; opt = Some
   t } *)

(* let is_letter (t : tile) = match t with 'a' .. 'z' | 'A' .. 'Z' ->
   Some t | _ -> None *)
