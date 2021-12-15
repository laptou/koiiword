type letter_deck = char list
(** A [letter_deck] represents a collection of uppercase letters
    available to the player to use. May not contain more than 7 letters
    at any time. *)

(* vowel_amounts is an association list that links each vowel to the
   probability of drawing that vowel given it's frequency in the tiles*)
let vowel_amounts : (float * char list) list =
  [
    (12.2, [ 'U' ]);
    (24.2, [ 'O' ]);
    (27.2, [ 'A'; 'I' ]);
    (36.4, [ 'E' ]);
  ]

(* consonant_amounts is an association list that links each consonant to
   the probability of drawing that consonant given it's frequency in the
   tiles*)
let consonant_amounts : (float * char list) list =
  [
    (6.2, [ 'Q'; 'Z'; 'J'; 'X'; 'K' ]);
    (12.5, [ 'F'; 'H'; 'V'; 'W'; 'Y'; 'B'; 'C'; 'M'; 'P' ]);
    (18.7, [ 'G' ]);
    (25.1, [ 'D'; 'S'; 'L' ]);
    (37.5, [ 'T'; 'R'; 'N' ]);
  ]

(** [get_random_item lst] is a random element from lst. *)
let get_random_item lst =
  let ind = Random.int (List.length lst) in
  List.nth lst ind

(** [match_probability prob lst] is a list of values from association
    list [lst] whose key satisfies the probability [prob]. Requires
    [prob] < 100 and [lst] to be a (float * char list) association list
    representing (probability * value list) tuples.*)
let match_probability prob lst =
  let rec inner_match_probability prob accum lst =
    match lst with
    | [] -> []
    | (p, ls) :: t ->
        let curr_prob = accum +. p in
        if prob <= curr_prob then ls
        else inner_match_probability prob curr_prob t
  in
  inner_match_probability prob 0.0 lst

(** [get_random_letter assoc] is a letter chosen using accurate
    probabilities from association list [assoc]*)
let get_random_letter assoc =
  let rand_pick = Random.float 100. in
  let letter_list = match_probability rand_pick assoc in
  get_random_item letter_list

(** [random_letter] is a randomly generated char from a-z. random_let :
    char. *)
let uniform_random_letter () = Char.chr (Random.int 26 + 65)

(** [realistic_letter] is a char, with a 19% chance of being a vowel. *)
let realistic_random_letter () =
  if Random.int 26 < 5 then get_random_letter vowel_amounts
  else get_random_letter consonant_amounts

(** [has_vowel lst] returns true if [lst] has a vowel and false
    otherwise*)
let rec has_vowel = function
  | [] -> false
  | h :: t ->
      if List.mem h [ 'A'; 'E'; 'I'; 'O'; 'U' ] then true
      else has_vowel t

(** [insert_vowel lst] replaces the first letter in [lst] with a
    randomly chosen vowel.*)
let insert_vowel lst =
  match lst with
  | [] -> []
  | _ :: t -> get_random_letter vowel_amounts :: t

(** [ensure_vowel lst] ensures that the lst has at least one vowel. If
    there is one vowel, [lst] is returned. Otherwise, an instance of
    [lst] with the first letter replaced with a vowel is returned.*)
let ensure_vowel lst = if has_vowel lst then lst else insert_vowel lst

(** [consume_letter letter deck] removes the first instance of [letter]
    from [deck]. Raises [Not_found] if [deck] does not contain [letter] *)
let consume_letter letter deck =
  let rec helper partial_lst =
    match partial_lst with
    | [] -> raise Not_found
    | h :: t -> if h = letter then t else h :: helper t
  in
  helper deck

let refill_deck deck =
  let rec helper partial_lst n =
    if n = 0 then partial_lst
    else realistic_random_letter () :: helper partial_lst (n - 1)
  in
  ensure_vowel (helper deck (7 - List.length deck))

(** [new_deck] is a letter_deck of length 7 reflecting accurate letter
    combinations. Probabilities for vowel/consonant frequency determined
    using http://www.breakingthegame.net/leaves2 *)
let new_deck () =
  let lst = refill_deck [] in
  ensure_vowel lst

let deck_from_letters deck = deck

let deck_to_letters deck = deck
