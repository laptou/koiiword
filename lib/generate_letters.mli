type letter_deck
(** letter_deck is a list containing 7 random chars at any time. To be
    used in a game of scrabble. If the char has been used in the game, a
    new char will take its place, so there will never be an empty index.
    Requires each char in the list to be an alphabetical letter at any
    given time. *)

val new_deck : unit -> letter_deck
(** [new_deck] is a letter_deck of length 7 reflecting accurate letter
    combinations. Probabilities for vowel/consonant frequency determined
    using http://www.breakingthegame.net/leaves2. Individual letter
    frequencies determined using
    https://en.wikipedia.org/wiki/Scrabble_letter_distributions*)

val consume_letter : char -> letter_deck -> letter_deck

val refill_deck : letter_deck -> letter_deck

val uniform_random_letter : unit -> char

val realistic_random_letter : unit -> char

val deck_to_letters : letter_deck -> char list

val deck_from_letters : char list -> letter_deck
