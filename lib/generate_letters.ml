type letter_deck = char list
(** letter_deck is a list containing 7 random chars at any time. To be
    used in a game of scrabble. If the char has been used in the game, a
    new char will take its place, so there will never be an empty index.
    Requires each char in the list to be an alphabetical letter at any
    given time. *)

(** [random_let] is a randomly generated char from a-z. random_let :
    char*)
let random_let () = Char.chr (Random.int 26 + 65)

(** [start_game lst] is a letter_deck of 7 chars. Requires lst to be
    empty since it is the start of the game. start_game : unit ->
    letter_deck *)
let rec start_game = function
  | [] -> start_game [ random_let () ]
  | h :: t ->
      if List.length t = 6 then h :: t
      else start_game (h :: random_let () :: t)

(** [remove_let curr goal lst] removes the appropriate letter when
    players use a letter and refills it with a new random letter.
    Requires curr to be the current index as the function progresses
    (starting at 0) and goal to be the index of the letter to be
    removed. remove_let : int -> int -> letter_deck -> letter_deck *)
let rec remove_let curr goal = function
  | [] -> start_game []
  | h :: t ->
      if curr = goal then random_let () :: t
      else h :: remove_let (curr + 1) goal t

(** [index_of_letter curr letter lst] is the index of the first
    occurrence of this character in the list. Requires letter to be
    present in lst and curr to be the current index (starting at 0).
    index_of_letter : int -> char -> letter_deck -> int*)
let rec index_of_letter curr letter = function
  | [] -> raise (Failure "Empty List")
  | h :: t ->
      if h = letter then curr else index_of_letter (curr + 1) letter t

(* vowels is a list containing every vowel char accepted by our Scrabble
   game*)
let vowels = [ 'A'; 'E'; 'I'; 'O'; 'U' ]

(* consonants is a list containing every consonant char accepted by our
   Scrabble game *)
let consonants =
  [
    'B';
    'C';
    'D';
    'F';
    'G';
    'H';
    'J';
    'K';
    'L';
    'M';
    'N';
    'P';
    'Q';
    'R';
    'S';
    'T';
    'V';
    'W';
    'X';
    'Y';
    'Z';
  ]

(* probability weights associated with each number of vowels in a
   starting hand. For implementer reference.*)
(**let weights =
   [ (0, 0.07); (1, 0.25); (2, 0.36); (3, 0.24); (4, 0.07); (5, 0.01) ]*)

(* alias table reference https://www.keithschwarz.com/darts-dice-coins/*)
(* probability list needed for an alias table*)
let prob = [ 0.42; 0.92; 0.56; 1.; 0.42; 0.06 ]

(* alias list needed for an alias table*)
let alias = [ 1; 2; 3; 0; 2; 2 ]

(** [biased prob total] is true for heads and false for tails. This
    method simulates a biased coin, with a [prob]/[total] chance of
    being heads. Requires [prob] <= [total].*)
let biased prob total =
  let pick = Random.int total in
  if pick < prob then true else false

(** [index_item ind curr lst] is the item of lst at index ind. The
    parameter curr is the current index.*)
let rec index_item curr ind = function
  | [] -> raise Not_found
  | h :: t -> if curr = ind then h else index_item (curr + 1) ind t

(** [get_rand lst] is a random element from lst. *)
let get_rand lst =
  let ind = Random.int (List.length lst) in
  index_item 0 ind lst

(** [num_vowels] is the number of vowels a player's starting hand based
    on a series of probabilities.*)
let num_vowels () =
  let side = Random.int 6 in
  let pick = index_item 0 side prob in
  let h_or_t = biased (int_of_float pick * 100) 100 in
  if h_or_t then side else index_item 0 side alias

(** [lst_of size lst repo] is a list of size [size] of elements from
    [repo]. Requires [lst] to be empty when first applied.*)
let rec lst_of size lst repo =
  match lst with
  | [] -> lst_of size [ get_rand repo ] repo
  | h :: t ->
      if List.length t = size - 1 then h :: t
      else lst_of size (h :: get_rand repo :: t) repo

(** [optimize_start lst] is a letter_deck of length 7 reflecting
    accurate letter combinations. Probabilities for vowel/consonant
    frequency determined using http://www.breakingthegame.net/leaves2 *)
let optimize_start () =
  let n_vowels = num_vowels () in
  let n_conson = 7 - n_vowels in
  lst_of n_vowels [] vowels @ lst_of n_conson [] consonants
