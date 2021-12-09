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

(** [wrap width str] returns a list of strings obtained by breaking
    [str] into lines no longer than [width] characters *)
let wrap (width : int) (str : string) : string list =
  let rec get_width line =
    match line with
    | [] -> 0
    | [ w ] -> String.length w
    | w :: t -> String.length w + 1 + get_width t
  in

  let words = String.split_on_char ' ' str in

  (* keep adding words to a line until doing so would overflow the line
     limit, then start a new line at the top of the list *)
  let lines =
    List.fold_left
      (fun lines word ->
        let (line_words :: remaining) = lines [@@warning "-8"] in
        if get_width (line_words @ [ word ]) > width then
          [ word ] :: lines
        else (line_words @ [ word ]) :: remaining)
      [ [] ] words
  in

  let lines =
    lines
    (* join words together using spaces to make lines, then split lines
       on \n and flatten *)
    |> List.rev_map (fun words ->
           words |> String.concat " " |> String.split_on_char '\n')
    |> List.flatten
  in
  lines


let rec range start stop step fn =
  fn start;
  if start + step < stop then range (start + step) stop step fn
