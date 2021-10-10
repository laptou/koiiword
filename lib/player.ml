(* Type for each player *)
type player = { letters : Generate_letters.letter_deck }

(** [advance_players player_lst] gives the same player list except the
    head player is moved to the last index. Returns empty list if empty
    list is given. Ex: [\[p1; p2; p3\]] -> [\[p2; p3; p1\]]*)
let advance_players (player_lst : player list) =
  match player_lst with [] -> [] | h :: t -> t @ [ h ]
