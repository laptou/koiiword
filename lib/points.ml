let letter_pts : (char list * int) list =
  [
    ([ 'A'; 'E'; 'I'; 'O'; 'U'; 'L'; 'N'; 'S'; 'T'; 'R' ], 1);
    ([ 'D'; 'G' ], 2);
    ([ 'B'; 'C'; 'M'; 'P' ], 3);
    ([ 'F'; 'H'; 'V'; 'W'; 'Y' ], 4);
    ([ 'K' ], 5);
    ([ 'J'; 'X' ], 8);
    ([ 'Q'; 'Z' ], 10);
  ]

(** [get_points ltr lst] is the point value associated with the letter
    [ltr] in the association list [lst]. Requires: ltr is an uppercase,
    alphabetic character and lst is a (char list * int) list. *)
let rec get_points ltr = function
  | [] -> 0
  | h :: t -> (
      match h with
      | let_lst, pt ->
          if List.mem ltr let_lst then pt else get_points ltr t)

(* [word_points wrd] is the number of points for wrd*)
let word_points wrd =
  let rec accumulate_points letters =
    match letters with
    | [] -> 0
    | h :: t -> get_points h letter_pts + accumulate_points t
  in
  accumulate_points (List.init (String.length wrd) (String.get wrd))
