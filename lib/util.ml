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

(** [set lst n item] will return a new list where the item at index [n]
    in [lst] is replaced with [item] and all other items are the same.
    If [n] is greater than or equal to the length of [lst], then [item]
    will be added at the end of the list. *)
let set (lst : 'a list) (n : int) (item : 'a) : 'a list =
  let left, right = slice lst n in
  match right with [] -> left @ [ item ] | _ :: t -> left @ (item :: t)

let explode s = List.init (String.length s) (String.get s)

let sign n = if n > 0 then 1 else if n < 0 then -1 else 0
