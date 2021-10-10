(* letter_deck is a list containing 7 random chars at any time. To be
   used in a game of scrabble. If the char has been used in the game, a
   new char will take its place, so there will never be an empty index.
   Requires each char in the list to be an alphabetical letter at any
   given time. *)
type letter_deck = char list

(* [start_game] is a letter_deck of length 7 reflecting accurate letter
   combinations. Probabilities for vowel/consonant frequency determined
   using http://www.breakingthegame.net/leaves2 . Individual letter
   frequencies determined using
   https://en.wikipedia.org/wiki/Scrabble_letter_distributions*)
val start_game : unit -> char list

(*[replace_let_biased goal lst] removes the appropriate letter when
  players use a letter and refills it with a new letter. The letter has
  a 19% chance of being a vowel. Requires goal to be the index of the
  letter to be removed. *)
val replace_let_biased : int -> letter_deck -> letter_deck

(* [start_game_deprecated lst] is a letter_deck of 7 random chars, with
   no probabilities baked in. Requires lst to be empty since it is the
   start of the game. start_game : unit \-> letter_deck *)
val start_game_deprecated : char list -> char list

val remove_let_deprecated : int -> int -> letter_deck -> letter_deck

val index_of_letter : int -> char -> letter_deck -> int

val random_let : unit -> char
