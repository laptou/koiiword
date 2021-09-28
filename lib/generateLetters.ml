type letter_deck = char list
(** letter_deck is a list containing 7 random chars at any time. To be
    used in a game of scrabble. If the char has been used in the game, a
    new char will take its place, so there will never be an empty index.
    Requires each char in the list to be an alphabetical letter at any
    given time. *)

(** [random_let] is a randomly generated char from a-z. random_let :
    char*)
let random_let bound = Random.int bound + 65 |> Char.chr

(** [start_game lst] is a letter_deck of 7 chars. Requires lst to be
    empty since it is the start of the game. start_game : unit ->
    letter_deck *)
let rec start_game = function
  | [] -> start_game [ random_let 26 ]
  | h :: t ->
      if List.length t = 6 then h :: t
      else start_game (h :: random_let 26 :: t)

(** [remove_let curr goal lst] removes the appropriate letter when
    players use a letter and refills it with a new random letter.
    Requires curr to be the current index as the function progresses
    (starting at 0) and goal to be the index of the letter to be
    removed. remove_let : int -> int -> letter_deck -> letter_deck *)
let rec remove_let curr goal = function
  | [] -> start_game []
  | h :: t ->
      if curr = goal then random_let 26 :: t
      else h :: remove_let (curr + 1) goal t

(** [index_of_letter curr letter lst] is the index of the first
    occurrence of this character in the list. Requires letter to be
    present in lst and curr to be the current index (starting at 0).
    index_of_letter : int -> char -> letter_deck -> int*)
let rec index_of_letter curr letter = function
  | [] -> raise (Failure "Empty List")
  | h :: t ->
      if h = letter then curr else index_of_letter (curr + 1) letter t
