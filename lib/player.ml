(* Type for each player in the list *)
type player = { letters : char list }

(** [nextplayer cur_player player_lst] gets the player after the player
    [cur_player] in list of players [player_lst]. If the current player
    is the last player in the list, the first element of the list is
    returned. *)
let next_player cur_player player_lst =
  let cur_lst = player_lst in
  let rec traverse list =
    match list with
    | [] -> failwith "Empty Player List"
    | h :: t when h = cur_player ->
        if t = [] then List.hd player_lst else List.hd t
    | _ :: t -> traverse t
  in
  traverse cur_lst
