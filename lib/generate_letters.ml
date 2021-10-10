(* letter_deck is a list containing 7 random chars at any time. To be
   used in a game of scrabble. If the char has been used in the game, a
   new char will take its place, so there will never be an empty index.
   Requires each char in the list to be an alphabetical letter at any
   given time. *)
type letter_deck = char list

(** [index_of_letter curr letter lst] is the index of the first
    occurrence of this character in the list. Requires letter to be
    present in lst and curr to be the current index (starting at 0).
    index_of_letter : int -> char -> letter_deck -> int*)
let rec index_of_letter curr letter = function
  | [] -> raise (Failure "Empty List")
  | h :: t ->
      if h = letter then curr else index_of_letter (curr + 1) letter t

(* vowel_amounts is an association list that links each vowel to the
   probability of drawing that vowel given it's frequency in the tiles*)
let vowel_amounts : (float * char list) list =
  [
    (12.1, [ 'U' ]);
    (24.2, [ 'O' ]);
    (27.2, [ 'A'; 'I' ]);
    (36.3, [ 'E' ]);
  ]

(* consonant_amounts is an association list that links each consonant to
   the probability of drawing that consonant given it's frequency in the
   tiles*)
let consonant_amounts : (float * char list) list =
  [
    (6.2, [ 'Q'; 'Z'; 'J'; 'X'; 'K' ]);
    (12.5, [ 'F'; 'H'; 'V'; 'W'; 'Y'; 'B'; 'C'; 'M'; 'P' ]);
    (18.7, [ 'G' ]);
    (25., [ 'D'; 'S'; 'L' ]);
    (37.5, [ 'T'; 'R'; 'N' ]);
  ]

(* alias table reference https://www.keithschwarz.com/darts-dice-coins/*)
(* probability list needed for an alias table*)
let prob = [ 0.42; 0.92; 0.56; 1.; 0.42; 0.06 ]

(* alias list needed for an alias table*)
let alias = [ 1; 2; 3; 0; 2; 2 ]

(** [biased_bool prob total] is true for heads and false for tails. This
    method simulates a biased coin, with a [prob]/[total] chance of
    being heads. Requires [prob] <= [total].*)
let biased_bool prob total =
  let pick = Random.int total in
  if pick < prob then true else false

(** [index_item ind curr lst] is the item of lst at index ind. The
    parameter curr is the current index.*)
let rec index_item curr ind = function
  | [] -> raise Not_found
  | h :: t -> if curr = ind then h else index_item (curr + 1) ind t

(** [get_random_item lst] is a random element from lst. *)
let get_random_item lst =
  let ind = Random.int (List.length lst) in
  index_item 0 ind lst

(** [inner_match_probability prob accum lst] is a helper for
    [match_probability]. Requires [accum] = 0*)
let rec inner_match_probability prob accum lst =
  match lst with
  | [] -> []
  | (p, ls) :: t ->
      let curr_prob = accum +. p in
      if prob <= curr_prob then ls
      else inner_match_probability prob curr_prob t

(** [match_probability prob lst] is the list of values in [lst] given
    probability [prob]. Requires [prob] < 100 and [lst] to be a (float *
    char list) association list representing (probability * value list)
    tuples.*)
let match_probability prob lst = inner_match_probability prob 0.0 lst

(** [get_random_letter assoc] is a letter chosen using accurate
    probabilities from association list [assoc]*)
let get_random_letter assoc =
  let rand_pick = Random.float 100. in
  let letter_list = match_probability rand_pick assoc in
  get_random_item letter_list

(** [inner_random_letters size assoc_ls lst] is a list of size [size]
    using the values and probabilities in [assoc_ls]. Requires [lst] to
    be empty upon application. Helper for [random_letters].*)
let rec inner_random_letters size assoc_ls lst =
  match lst with
  | [] ->
      inner_random_letters size assoc_ls [ get_random_letter assoc_ls ]
  | h :: t ->
      if List.length t = size - 1 then h :: t
      else
        inner_random_letters size assoc_ls
          (h :: get_random_letter assoc_ls :: t)

(** [random_letters size assoc] is a list of size [size] filled with
    letters by accurate probability metrics from [assoc]*)
let rec random_letters size assoc = inner_random_letters size assoc []

(** [num_vowels] is the number of vowels a player's starting hand based
    on a series of probabilities.*)
let num_vowels () =
  let side = Random.int 6 in
  let pick = index_item 0 side prob in
  let h_or_t = biased_bool (int_of_float pick * 100) 100 in
  if h_or_t then side else index_item 0 side alias

(** [start_game] is a letter_deck of length 7 reflecting accurate letter
    combinations. Probabilities for vowel/consonant frequency determined
    using http://www.breakingthegame.net/leaves2 *)
let start_game () =
  let n_vowels = num_vowels () in
  let n_conson = 7 - n_vowels in
  random_letters n_vowels vowel_amounts
  @ random_letters n_conson consonant_amounts

(** [realistic_let] is a char, with a 19% chance of being a vowel. *)
let realistic_let () =
  if Random.int 26 < 5 then
    index_item 0 0 (random_letters 1 vowel_amounts)
  else index_item 0 0 (random_letters 1 consonant_amounts)

(** [inner_let_biased curr goal lst] removes the appropriate letter when
    players use a letter and refills it with a new letter. The letter
    has a 19% chance of being a vowel. Requires curr to be the current
    position (starting at 0) goal to be the index of the letter to be
    removed. Helper for [replace_let_biased]*)
let rec inner_let_biased curr goal = function
  | [] -> start_game ()
  | h :: t ->
      if curr = goal then realistic_let () :: t
      else h :: inner_let_biased (curr + 1) goal t

(** [replace_let_biased goal lst] removes the appropriate letter when
    players use a letter and refills it with a new letter. The letter
    has a 19% chance of being a vowel. Requires goal to be the index of
    the letter to be removed. *)
let rec replace_let_biased goal lst = inner_let_biased 0 goal lst

(** [inner_random_items size lst repo] is a list of size [size] of
    elements from [repo]. Helper for [random_items]*)
let rec inner_random_items size lst repo =
  match lst with
  | [] -> inner_random_items size [ get_random_item repo ] repo
  | h :: t ->
      if List.length t = size - 1 then h :: t
      else inner_random_items size (h :: get_random_item repo :: t) repo

(** [random_items size repo] is a list of size [size] of elements from
    [repo].*)
let rec random_items size repo = inner_random_items size [] repo

(** [random_let] is a randomly generated char from a-z. random_let :
    char*)
let random_let () = Char.chr (Random.int 26 + 65)

(** [start_game_deprecated lst] is a letter_deck of 7 chars. Requires
    lst to be empty since it is the start of the game. start_game : unit
    \-> letter_deck *)
let rec start_game_deprecated = function
  | [] -> start_game_deprecated [ random_let () ]
  | h :: t ->
      if List.length t = 6 then h :: t
      else start_game_deprecated (h :: random_let () :: t)

(** [remove_let_deprecated curr goal lst] removes the appropriate letter
    when players use a letter and refills it with a new random letter.
    Requires curr to be the current index as the function progresses
    (starting at 0) and goal to be the index of the letter to be
    removed. remove_let : int -> int -> letter_deck -> letter_deck *)
let rec remove_let_deprecated curr goal = function
  | [] -> start_game_deprecated []
  | h :: t ->
      if curr = goal then random_let () :: t
      else h :: remove_let_deprecated (curr + 1) goal t
