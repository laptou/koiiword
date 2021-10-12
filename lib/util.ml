(** [slice lst n] will return two lists [(left, right)], where [left]
    contains items 0 until [n] from [lst] and [right] contains all
    remaining items. *)
let slice (lst : 'a list) (n : int) : 'a list * 'a list =
  let rec slice_inner
      (lst_a : 'a list)
      (lst_b : 'a list)
      (n : int)
      (i : int) : 'a list * 'a list =
    if n = i then (lst_a, lst_b)
    else
      match lst_b with
      | [] -> (lst_a, [])
      | h :: t -> slice_inner (lst_a @ [ h ]) t n (i + 1)
  in
  slice_inner [] lst n 0
