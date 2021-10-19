type letter_deck = char list
(** [letter_deck] is a list containing 7 random chars at any time. To be
    used in a game of scrabble. If the char has been used in the game, a
    new char will take its place, so there will never be an empty index.
    Requires each char in the list to be an alphabetical letter at any
    given time. *)

(** [random_let] is a randomly generated char from a-z. random_let :
    char*)
let random_let () = Char.chr (Random.int 26 + 65)

(** [new_deck ()] returns a letter_deck of 7 chars. *)
let new_deck () =
  let rec new_deck_inner = function
    | [] -> new_deck_inner [ random_let () ]
    | h :: t ->
        if List.length t = 6 then h :: t
        else new_deck_inner (h :: random_let () :: t)
  in
  new_deck_inner []

exception Missing_letter

(** [remove_let curr goal lst] removes the appropriate letter when
    players use a letter and refills it with a new random letter.
    Requires curr to be the current index as the function progresses
    (starting at 0) and goal to be the index of the letter to be
    removed. remove_let : int -> int -> letter_deck -> letter_deck *)
let rec replace_letter letter letter_deck =
  match letter_deck with
  | h :: t ->
      if h = letter then
        let new_letter = random_let () in
        new_letter :: t
      else h :: replace_letter letter t
  | [] -> raise Missing_letter

(** used http://www.breakingthegame.net/leaves2 to generate accurate
    probability rates for vowels and consonants*)
