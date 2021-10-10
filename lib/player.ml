(* Type for each player *)
type player = { letters : char list }

(** [update_player_list player_lst] gives a player list with the second
    player becoming the head player. Returns the empty list if empty
    list is given. *)
let update_player_list (player_lst : player list) =
  match player_lst with [] -> [] | h :: t -> t @ [ h ]
